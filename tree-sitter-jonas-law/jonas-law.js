module.exports = grammar({
    name: 'jonas_law',
    rules: {
        source_file: $ => repeat1($.clause),
        clause: $ => seq(choice($.head, $.rule), '.'),
        head: $ => choice($.atom, $.compound),
        rule: $ => seq($.head, ':-', $.term_list),
        term: $ => choice($.atom, $.variable, $.number, $.string, $.date, $.compound),
        term_list: $ => seq($.term, repeat(seq(',', $.term))),
        atom: $ => /\p{Lowercase}[\p{Alphabetic}\p{Number}_$+*/-]*/,
        compound: $ => seq(field('functor', $.atom), '(', $.term_list, ')'),
        variable: $ => /[\p{Uppercase}_][\p{Alphabetic}\p{Number}+*/-]*/,
        number: $ => /[0-9]+/,
        string: $ => /'[^']*'/,
        date: $ => /#d'[^']*'/
    }
});
