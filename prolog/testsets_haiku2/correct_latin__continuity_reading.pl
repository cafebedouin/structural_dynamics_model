% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Correct Latin as Living Transmitted Practice (Continuity Reading)
 *   domain: intellectual/linguistic/institutional
 *
 * SUMMARY:
 *   The continuity reading of correct Latin claims that legitimate Latin
 *   forms are those transmitted through continuous institutional
 *   practice—what medieval clergy inherited from their teachers, what they
 *   use in liturgy and administration, what their manuscripts instantiate.
 *   This reading emerges from within the medieval ecclesiastical institutions
 *   and grounds correctness in transmission authority rather than textual
 *   restoration. It stands against the discontinuity reading (medieval Latin
 *   is corrupt deviation from Classical purity) and the hybrid reading (both
 *   Classical and medieval forms carry authority, with selective correction).
 *   The kernel itself—'what is correct Latin?'—is stabilized through
 *   ecclesiastical transmission lineages, but the reading is contested:
 *   humanist scholars and textual reconstructionists reject the claim that
 *   institutional practice, disconnected from ancient sources, defines
 *   correctness.
 *
 * KEY AGENTS:
 *   - Ecclesiastical institutions (Church, dioceses, monasteries): agenda-setter, enforcer, beneficiary. Sets institutional teaching standards, controls manuscript production, enforces liturgical Latin forms.
 *   - Medieval practitioners (clergy, monks, scribes): beneficiary, payer. Inherit and use the living practice; benefit from legitimacy without needing textual justification; pay the cost of diverging from Classical forms without explicit permission to do so.
 *   - Classical purists (humanist scholars, revival advocates): payer, observer. Claim true correctness is Classical; under this reading their standard is displaced and their restoration project is declared artificial.
 *   - Textual reconstructionists (philologists): payer, observer. Use systematic textual analysis to recover ancient forms; under this reading their method is subordinated to institutional transmission authority.
 *   - Oral transmission authority (the lineage structure itself, not a person): beneficiary, analytical. The structure that grounds legitimacy in teacher-to-student succession gains authority from this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Correct Latin as Living Transmitted Practice (Continuity Reading)").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "intellectual/linguistic/institutional").

domain_priors:requires_active_enforcement(correct_latin__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, 'b3d1f617-5f07-4c4f-8786-aa35b6ff69eb').
narrative_ontology:cs_kernel_codification('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', distributed).
narrative_ontology:cs_authority_grounding('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', lineage).
narrative_ontology:cs_interpretation_layer_present('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb').
narrative_ontology:cs_reading_relation('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', foundational, transmitted_practice_is_authoritative).
narrative_ontology:cs_axiom_status(transmitted_practice_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', transmitted_practice_is_authoritative, conventional).
narrative_ontology:cs_axiom('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', foundational, rupture_with_classical_is_legitimate_evolution).
narrative_ontology:cs_axiom_status(rupture_with_classical_is_legitimate_evolution, holdable).
narrative_ontology:cs_axiom_grounding('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', rupture_with_classical_is_legitimate_evolution, conventional).
narrative_ontology:cs_reference_frame('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', institutional_transmission_supremacy).
narrative_ontology:cs_drift_state('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', high_medieval_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3d1f617-5f07-4c4f-8786-aa35b6ff69eb', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_practitioners).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, oral_transmission_authority).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classical_purists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, textual_reconstructionists).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, excluded_alternative_readings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy, monks, and institutional scribes who inherit and maintain living Latin practice through daily use in liturgy, administration, and manuscript production. Their correctness standard is what they learned from their teachers and what keeps their institutional communications functional. They benefit from the reading's legitimacy: their own evolved forms are declared correct without requiring defensive justification against an external textual authority.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_practitioners, beneficiary,
    organized, generational, constrained, regional).

% The Church, dioceses, and monastic orders that enforce and transmit Latin through institutional teaching, liturgical standardization, and manuscript production. They set the agenda for what counts as correct by deciding which texts are authoritative in their schools, which forms are mandated in the liturgy, and which scribal practices are institutionalized. They benefit from stability of the living practice frame: it anchors correctness in their own authority and teaching lineages, not in external textual reconstruction.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Not a person or institution but the authority structure that grounds legitimacy in continuous oral-and-written transmission from teacher to student, master to apprentice, monastic lineage to lineage. It benefits by being declared the authoritative source of correctness: what the living practice transmits is correct by definition.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, oral_transmission_authority, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__continuity_reading, oral_transmission_authority).

% Humanist scholars, revival advocates, and philologists who argue that true correctness is Classical Latin as attested in ancient texts and inscriptions. Under the continuity reading, their forms are declared deviant or archaic, their project to restore Classical precision is framed as artificial reconstruction, and their authority to judge correctness is displaced by institutional practice. They bear the cost of having their standard dismissed as external nostalgia rather than legitimate restoration.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classical_purists, payer,
    powerful, biographical, mobile, regional).

% Scholars who claim to recover correct Latin through systematic textual analysis—comparing manuscripts, identifying scribal corruption, and establishing ancient forms through philological method. Under the continuity reading, their method is subordinated: the living practice of the medieval institutions is declared authoritative even where it diverges from the reconstructed text, making textual authority secondary to institutional transmission. They pay the epistemic cost of having systematic textual evidence ranked below institutional practice.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_reconstructionists, payer,
    powerful, biographical, mobile, continental).

% Readers and practitioners who would advocate for the discontinuity reading (medieval forms are corrupt deviations) or the hybrid reading (both Classical and medieval forms carry authority with selective correction possible). They are not in the institutional transmission lineage that authorizes the continuity reading, and their voices are structurally absent from the academy's consensus formation—not forbidden, but institutionally unheard because the transmission lineage does not invite them.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, excluded_alternative_readings, excluded,
    organized, biographical, constrained, regional).

% Those who systematize and codify Latin grammar—composing grammars, teaching rules, setting pedagogical standards. Their seat is somewhat orthogonal to the reading contest: they observe and describe what forms are in use, but the reading determines whether they describe those forms as legitimate or deficient.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, professional_grammarians, observer,
    institutional, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable standard for correct Latin that does not require continuous appeal to inaccessible ancient sources: practitioners know correctness through their training and institutional context, not through philological expertise. Solves the coordination problem of multi-generational institutional communication without requiring every speaker to be a textual scholar.
% TRANSFER_FUNCTION: Transfers authority over correctness from external textual sources (ancient texts, reconstructed forms) to the living institutional transmission—from the humanist scholar's desk to the monastery's teaching lineage. This movement of authority also transfers deference: institutional practitioners gain status as the legitimate arbiters of correctness, while purists lose the authority they claim through textual expertise.
% ABSENT_VOICES: Discontinuity advocates (medieval forms are corrupt) and hybrid advocates (selective correction possible) are absent from the reading's own transmission lineage—they do not train students in the continuity framework, so their objections appear external to the system rather than alternative readings from within it. Practitioners outside the Church's institutional chain are also absent: secular scribal traditions, folk usage, and lay literacy do not feed into the authoritative transmission lineage this reading elevates.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished and practitioners could no longer claim their evolved forms are legitimate, institutional Latin would face a crisis of correctness: each generation would need to either justify medieval forms through external textual authority (the discontinuity frame) or undertake systematic restoration (the hybrid frame). The institutional stability that rests on 'this is what we inherited' would collapse, forcing either radical conservatism (revert to reconstructed Classical forms) or explicit reform.
% FOUNDING_PROBLEM: After Classical Latin's decline in the post-Roman centuries, a writing system persists but texts accumulate variation from local speech, scribal practice, and liturgical innovation. Without a standard, institutional communication across regions and generations becomes unreliable. The reading solves this by declaring the transmitted practice itself the standard, rather than requiring constant reconstruction from ancient texts.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical historians and institutional scholars testify that the continuity frame enabled multi-generational stability in the Church's administration and liturgy. Discontinuity advocates counter that the 'stability' was actually drift, and that the founding problem was never solved—only masked by declaring deviation legitimate. Independent philological analysis supports both positions depending on what constitutes a solution: stable institutional practice or fidelity to attested Classical forms.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the constraint's asymmetric transfer of authority from external textual sources to institutional transmission: purists and reconstructionists lose the right to judge correctness by their methods; institutional practitioners gain the right without external validation. The measurement rises from 0.45 to 0.62 over the interval, tracking the intensifying reach of the reading's authority—from early medieval period when textual authority was still occasionally invoked, to high medieval period when institutional practice became the settled standard across most regions. Suppression is high (0.71 by interval end) because maintaining this reading requires actively defending the institutional transmission lineage against external criticism (textual evidence, Classical forms) and discouraging appeal to reconstructed sources. Theater ratio is moderate (0.48): genuine institutional function (stable communication through shared practice) is real, but a growing share of the reading's enforcement activity defends the frame itself against alternative readings rather than serving its original coordination function. Accessibility collapse is moderate (0.64): alternatives (Classical restoration, textual reconstruction) are theoretically available to any educated person with manuscripts, but the institutional suppression of their legitimacy makes them practically inaccessible within the Church's dominant academic and scribal structures.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence tracks the asymmetry between institutional power and scholarly autonomy. The Church controls the resources (scriptoria, teaching positions, legitimacy in public discourse), and it uses those resources to enforce the continuity reading. Purists have intellectual mobility but institutional exclusion: they can be right in principle but cannot publish credibly within the dominant academic structure. Practitioners have neither: their forms are legitimate but only because the institution says so, and they cannot learn to question that legitimacy without leaving their profession entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval practitioners sit at d ≈ 0.50–0.65: they are beneficiaries (legitimacy without external justification) but identity-locked (their entire training, career, and institutional identity is bound to the transmitted practice). This moderates their beneficiary directionality because escape is neither attractive nor feasible—the benefit is real but comes at the cost of total institutional capture. Ecclesiastical institutions sit at d ≈ 0.10–0.20: pure beneficiary (sets rules, collects authority, maintains transmission lineages). Classical purists sit at d ≈ 0.80–0.90: targets (lose textual authority, are actively discredited, have constrained exit—publishing classical reconstruction within ecclesiastical institutions invites suppression). Oral transmission authority is a non-agent (d does not apply) but benefits structurally from the reading's legitimation of its own authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Roman Latin variation threatens institutional communication) is live throughout the interval. But the solution the continuity reading provides is increasingly performative by the high medieval period. Institutional communication is stable not because the reading solved the problem, but because institutional practice had ossified—the transmitted forms became standardized through repetition and enforcement, not through the reading's legitimating function. By interval end, the reading's primary activity is defending itself against external criticism (suppression_requirement rises), not solving the original coordination problem. This marks the constraint's shift toward piton-hood: the reading persists because the institutions have invested in it, not because it solves an ongoing problem. The mandatrophy resolution is partial: the reading did solve the problem it was built for, but the solution enabled new extraction (denial of Classical authority, suppression of alternative methods) that persists long after the original crisis was past.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_lineage_authority,
    'Is the authority of the continuity reading grounded in the epistemic value of continuous transmission (the reading captures something true about how language works), or in the institutional power of the ecclesiastical institutions (transmission is authoritative because the Church controls it)?',
    'Comparative historical analysis of non-ecclesiastical Latin lineages (secular administrative Latin, folk literacies, trade Latin) to test whether continuous transmission without institutional enforcement also produces stable forms, and whether those forms align with or diverge from institutional medieval Latin.',
    'If non-ecclesiastical transmission produces similar evolution, the reading''s epistemic content is defensible—continuity itself generates correctness. If secular lineages diverge significantly, the reading''s apparent universality conceals institutional capture, and correctness is institutional decree, not transmitted legitimacy. This changes classification from tangled_rope (genuine coordination + extraction) toward snare (pure enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_lineage_authority, empirical, 'Whether transmission authority is epistemic or institutional.').

omega_variable(
    kernel_reading_vs_natural_law,
    'Is correct Latin a fact about language (some reading of the kernel is objectively true) or a choice (all three readings—continuity, discontinuity, hybrid—are equally defensible frameworks, and the reading''s persistence reflects institutional power rather than truth)?',
    'Philosophical analysis of the criteria for correctness in language: if correctness is defined by usage, the continuity reading is correct by definition (institutional usage becomes the standard). If correctness is defined by ancient attestation, the discontinuity reading is correct by definition. If correctness is subject to systematic reform, the hybrid reading is correct. No empirical data can resolve which criterion is right—this is a conceptual question about what ''correct language'' means.',
    'If correctness is usage, the reading is self-vindicating and the constraint approaches mountain-hood (it is how language works). If correctness is attestation or reformability, the constraint is pure choice, and classification shifts toward snare (enforcement of one reading against alternatives that are equally legitimate). This affects whether mandatrophy is resolved (if self-vindicating, no mandatrophy; if enforced choice, mandatrophy is chronic).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_vs_natural_law, conceptual, 'Whether correctness in language is a fact or a choice.').

omega_variable(
    discontinuity_reading_foreclosure,
    'Does the continuity reading logically foreclose the discontinuity reading (no single framework could hold both), or do they merely coexist as different parties'' frameworks?',
    'Logical analysis: if ''correct Latin is what is transmitted'' and ''correct Latin is what is Classical'' can both be true in some coherent meta-framework (e.g., ''correct Latin has two components: transmitted practice and Classical attestation''), then they coexist. If neither statement can be true without the other being false, they foreclose.',
    'Foreclosure would justify classifying the reading_relations as ''forecloses''; coexistence would justify ''coexists_with''. This affects whether the engine''s foreclosure detection triggers and whether the sibling reading is expected to be live in the corpus or overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discontinuity_reading_foreclosure, conceptual, 'Whether continuity and discontinuity readings are logically incompatible.').

omega_variable(
    suppression_internalization,
    'What fraction of the measured suppression (0.71) is structural (institutional barriers to accessing Classical texts, teaching only transmitted forms) versus internalized (practitioners have been taught not to question the transmitted practice, identity-fused with institutional Latin)?',
    'Post-exit trajectory: in periods or regions where practitioners acquired Classical education (e.g., some Renaissance-era clergy who were trained in both traditions), did the suppression persist (suggesting internalization) or dissolve (suggesting purely structural suppression)? If it persisted, practitioners continue to treat transmitted forms as correct even when Classical alternatives are available, indicating identity lock.',
    'If internalized dominates, the constraint''s effective suppression is higher than the structural measure suggests—practitioners carry the suppression with them beyond institutional exit, and remedies focused on removing structural barriers would be insufficient. If structural dominates, opening access to Classical texts and alternative teaching lineages would dissolve the reading''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Internalization vs. structural suppression in the continuity reading''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(corr_tr_t5, correct_latin__continuity_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(corr_tr_t10, correct_latin__continuity_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(corr_tr_t15, correct_latin__continuity_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(corr_tr_t20, correct_latin__continuity_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(corr_tr_t25, correct_latin__continuity_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement(corr_tr_t30, correct_latin__continuity_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(corr_tr_t40, correct_latin__continuity_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(corr_be_t5, correct_latin__continuity_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(corr_be_t10, correct_latin__continuity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(corr_be_t15, correct_latin__continuity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(corr_be_t20, correct_latin__continuity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(corr_be_t25, correct_latin__continuity_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(corr_be_t30, correct_latin__continuity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(corr_be_t40, correct_latin__continuity_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(corr_su_t5, correct_latin__continuity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(corr_su_t10, correct_latin__continuity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(corr_su_t15, correct_latin__continuity_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(corr_su_t20, correct_latin__continuity_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(corr_su_t25, correct_latin__continuity_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(corr_su_t30, correct_latin__continuity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(corr_su_t40, correct_latin__continuity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(correct_latin__continuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The 'correct Latin' kernel admits at least three structurally distinct readings. This story instantiates the continuity reading: correctness grounds in institutional transmission. The discontinuity reading (constraint_id: correct_latin__discontinuity_reading) grounds in Classical textual attestation and treats medieval forms as deviations. The hybrid reading (constraint_id: correct_latin__hybrid_reading) grounds in Classical forms as transmitted through medieval practice, with permissible selective correction via textual evidence. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The three are linked via network.affects_constraints: the continuity reading's enforcement of transmission authority directly suppresses the discontinuity and hybrid readings' alternative claims to legitimacy. The readings are thus not independent constraints but a family, with the continuity reading as the institutional enforcer and the other readings as suppressed alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__continuity_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
