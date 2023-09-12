const jonas_law = require('./jonas-law');

module.exports = grammar(jonas_law, {
    name: 'jonas_law_explanations',
    rules: {
        clause: $ => seq($.compound, ';', $.explanation),
        explanation: $ => /.*/
    }
});
