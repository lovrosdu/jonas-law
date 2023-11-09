module.exports = grammar({
    name: 'jonas_law',
    rules: {
        source_file: $ => repeat1($.clause),
        clause: $ => seq($.rule, repeat(seq(':',$.reason)),';'),
	reason: $ => /[^;]*/,
        rule: $ => choice(seq($.head,'<-'), seq($.head, '<-', $.body)),
        head: $ => $.atom,
	body: $ => seq($.atomf, repeat(seq(',',$.atomf))),
	atomf: $ => choice($.atomn, $.atomd, $.atomc),
	atomn: $ => choice($.atom, seq('~', $.atom)),
	atomd: $ => choice(seq($.atomn, '\\/', $.atomn), seq($.atomn, '\\/', $.atomd), seq('[', $.atomc, ']'), seq('~[', $.atomd, ']'), seq('[', $.atomc, ']', '\\/', '[', $.atomc, ']')),
	atomc: $ => choice(seq($.atomn, '/\\', $.atomn), seq($.atomn, '/\\', $.atomc), seq('[', $.atomd, ']'), seq('~[', $.atomc, ']'), seq('[', $.atomd, ']', '/\\', '[', $.atomd, ']')),
        atom: $ => choice(seq($.predicate, '(', $.term_list, ')')),
        term: $ => choice($.variable, $.constantf),
        term_list: $ => seq($.term, repeat(seq(',', $.term))),
	constantf: $ => choice($.constant, $.constantd, $.constantc),
	constantd: $ => choice(seq($.constant, '\\/', $.constant), seq($.constant, '\\/', $.constantd), seq('[', $.constantc, ']')),
	constantc: $ => choice(seq($.constant, '/\\', $.constant), seq($.constant, '/\\', $.constantc), seq('[', $.constantd, ']')),
	constant: $ => choice($.number, $.lstring),
	variable: $ => $.ustring,
        predicate: $ => choice($.pstring, $.symbol),
	lstring: $ => /[^\p{Uppercase}\[,][^)(,;\\:\[\]~]*/,
        ustring: $ => /[\p{Uppercase}][^)(,;\\/\[\]~]*/,
	pstring: $ => /[\p{L}&#\p{Number}]+/,
	symbol: $ => /[<+/=-]/,
	number: $ => /\p{Number}/
    }
});
