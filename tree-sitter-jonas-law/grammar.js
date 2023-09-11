module.exports = grammar({
    name: 'jonas_law',
    rules: {
        source_file: $ => repeat1($.clause),
        clause: $ => seq(choice($.head, $.rule), '.'),
        head: $ => choice($.atom, $.operator),
        rule: $ => seq($.head, ':-', $.term_list),
        term: $ => choice($.atom, $.variable, $.number, $.string, $.date, $.operator),
        term_list: $ => seq($.term, repeat(seq(',', $.term))),
        atom: $ => /[a-z][a-zA-Z0-9_]*/,
        operator: $ => seq($.atom, '(', $.term_list, ')'),
        variable: $ => /[A-Z][a-zA-Z0-9_]*/,
        number: $ => /[0-9][0-9]*/,
        string: $ => /'[^']*'/,
        date: $ => /#d'[^']*'/
    }
});
