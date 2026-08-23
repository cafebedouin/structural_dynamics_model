% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal-Equivalence Translation Regime (Source-Structure Fidelity Primary)
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   Across hermeneutically conservative Christianity, translation policy
 *   binds vernacular scripture to the structure of the Hebrew, Aramaic, and
 *   Greek source texts: word order, idiom, and syntactic relations are
 *   preserved even at the price of readability, and the resulting difficulty
 *   is assigned to readers and their communities, whose responsibility is to
 *   be taught. The arrangement is administered by ordained teaching offices,
 *   financed through seminary language training, and policed by
 *   denominational translation mandates. This story instantiates the
 *   formal_equivalence_reading of the kernel biblical_source_text as a clean,
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the formal-equivalence regime itself, and epsilon is authored for that
 *   regime as encountered by this reading's own lights — the reading endorses
 *   the reader burden as the price of fidelity, while the structural flows
 *   (education costs, mediated access, accrued interpretive authority) are
 *   recorded as they actually run; the endorsement-versus-flow gap is carried
 *   by the formation_or_extraction omega rather than smoothed away. Sibling
 *   readings (dynamic_equivalence_reading, critical_reconstructive_reading)
 *   are separate constraint stories with their own epsilon, beneficiaries,
 *   and victims; the family decomposition is recorded in
 *   network.dual_formulation_note. KEY AGENTS (by structural relationship): -
 *   teaching_office_clergy: Primary beneficiary and agenda-setter
 *   (institutional/identity_locked) — administers teaching-mediated access;
 *   authority rests on bridging the text -
 *   seminary_biblical_language_faculty: Secondary beneficiary
 *   (organized/constrained) — supplies the training the access structure
 *   makes mandatory - hermeneutically_conservative_denominations:
 *   Institutional agenda-setter and beneficiary (institutional/constrained) —
 *   licenses translations, collects cohesion from textual stability -
 *   untrained_lay_congregants: Primary target (moderate/constrained) — bears
 *   education costs or permanent mediation -
 *   mission_field_convert_communities: Target (powerless/trapped) — meets the
 *   text where teaching infrastructure is thinnest -
 *   oral_culture_low_literacy_communities: Heaviest-burdened target
 *   (powerless/trapped) - dynamic_translation_advocates: Excluded voice
 *   (powerful/mobile) — would compete on intelligibility; kept outside the
 *   conversation - academic_textual_critics: Analytical observer
 *   (institutional/analytical) — sees the full structure, including the
 *   fragility of the determinacy premise
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.7).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.55).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal-Equivalence Translation Regime (Source-Structure Fidelity Primary)").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, '5fdc3794-057b-40dc-a33a-f1b7a2286eec').
narrative_ontology:cs_kernel_codification('5fdc3794-057b-40dc-a33a-f1b7a2286eec', fixed_text).
narrative_ontology:cs_authority_grounding('5fdc3794-057b-40dc-a33a-f1b7a2286eec', lineage).
narrative_ontology:cs_interpretation_layer_present('5fdc3794-057b-40dc-a33a-f1b7a2286eec').
narrative_ontology:cs_reading_relation('5fdc3794-057b-40dc-a33a-f1b7a2286eec', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fdc3794-057b-40dc-a33a-f1b7a2286eec', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('5fdc3794-057b-40dc-a33a-f1b7a2286eec', foundational, verbal_inspiration_attaches_to_source_words).
narrative_ontology:cs_axiom_status(verbal_inspiration_attaches_to_source_words, holdable).
narrative_ontology:cs_axiom_grounding('5fdc3794-057b-40dc-a33a-f1b7a2286eec', verbal_inspiration_attaches_to_source_words, theological).
narrative_ontology:cs_axiom('5fdc3794-057b-40dc-a33a-f1b7a2286eec', foundational, intelligibility_is_community_teaching_duty).
narrative_ontology:cs_axiom_status(intelligibility_is_community_teaching_duty, holdable).
narrative_ontology:cs_axiom_grounding('5fdc3794-057b-40dc-a33a-f1b7a2286eec', intelligibility_is_community_teaching_duty, deontological).
narrative_ontology:cs_axiom('5fdc3794-057b-40dc-a33a-f1b7a2286eec', secondary, translator_discretion_minimization).
narrative_ontology:cs_axiom_status(translator_discretion_minimization, holdable).
narrative_ontology:cs_axiom_grounding('5fdc3794-057b-40dc-a33a-f1b7a2286eec', translator_discretion_minimization, instrumental).
narrative_ontology:cs_reference_frame('5fdc3794-057b-40dc-a33a-f1b7a2286eec', stable_determinate_source_text).
narrative_ontology:cs_drift_state('5fdc3794-057b-40dc-a33a-f1b7a2286eec', contemporary_textual_criticism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fdc3794-057b-40dc-a33a-f1b7a2286eec', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, teaching_office_clergy).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, seminary_biblical_language_faculty).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, untrained_lay_congregants).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, mission_field_convert_communities).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, oral_culture_low_literacy_communities).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, verbal_plenary_inspiration_doctrine).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, original_text_determinacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordained pastors, priests, and teachers who decide which translations are read, preach with reference to the original languages, and run the catechetical programs through which members learn what the text means. Their standing in the community rests on being the ones who can bridge the ancient languages; a member who learned Greek privately would route around them. Leaving the role would mean leaving the vocation and self-understanding they were formed in.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, teaching_office_clergy, agenda_setter,
    institutional, generational, identity_locked, global).

% Professors of Hebrew, Aramaic, and Greek who train the clergy. Their departments, employment, and scholarly prestige depend on language study remaining a required gateway, and enrollment follows whatever the denominations mandate. Their skills are specialized enough that moving to another field would forfeit most of their career capital.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, seminary_biblical_language_faculty, beneficiary,
    organized, biographical, constrained, continental).

% Denominational bodies that adopt official translation policies, license which versions may be read in worship, and discipline deviations. Textual stability gives them a fixed object around which to organize doctrine and a durable boundary-marker against looser traditions. Changing translation philosophy would fracture their constituencies, so policy moves slowly even when scholars inside them push for it.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, hermeneutically_conservative_denominations, beneficiary).

% Members who read the scriptures only in translation and depend on sermons, study guides, and teachers for anything beyond surface comprehension. Direct access would require years of language study most cannot undertake alongside work and family. They can switch to communities using more readable translations, at the cost of relationships, identity, and sometimes employment tied to the congregation.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, untrained_lay_congregants, payer,
    moderate, biographical, constrained, global).

% Newly converted communities encountering the scriptures through formal-equivalence translations before any teaching infrastructure has reached them. They cannot yet evaluate what they are told, and the surrounding congregation is often the only access route they have; leaving it means losing the text altogether.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, mission_field_convert_communities, payer,
    powerless, biographical, trapped, regional).

% Communities for whom the prescribed remedy — personal study of ancient languages, or even sustained private reading — presupposes literacy and leisure they do not have. The heaviest costs of the access structure land here, and translation-policy conversations rarely include anyone from these seats.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, oral_culture_low_literacy_communities, payer,
    powerless, generational, trapped, regional).

% Translation theorists and mission agencies arguing that a translation should communicate in the target language, with intelligibility as the translator's responsibility. Inside formal-equivalence communities their proposals are received as invitations to infidelity rather than as policy options, so they publish, train, and distribute through parallel institutions instead.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_translation_advocates, excluded,
    powerful, biographical, mobile, global).

% Scholars who reconstruct the source text from manuscripts and document how much judgment the reconstruction requires. They study the whole arrangement from outside any confessional allegiance, including how fragile the notion of a single determinate original turns out to be.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, academic_textual_critics, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__formal_equivalence_reading, teaching_office_clergy).
narrative_ontology:fixing_cost_class(biblical_source_text__formal_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single stable reference text across a geographically and generationally dispersed community: binding translation to source-language structure limits each translator's discretionary insertions, gives scattered congregations a common object against which teaching can be checked, and preserves semantic density (idiom, allusion, legal formula) that free rendering flattens. It also coordinates the training pipeline: a stable difficult text gives language study a fixed curriculum.
% TRANSFER_FUNCTION: Moves interpretive labor and deference from untrained readers to the credentialed teaching office: lay readers transfer years of study-time or accept mediated meaning; tuition flows to seminaries; interpretive status accrues to language-holders. Money and status move upward; the text itself moves not at all — that immobility is the point.
% ABSENT_VOICES: Dynamic-equivalence translators and mission-field practitioners stand outside the conversation: their intelligibility findings enter only as cautionary tales about infidelity. Oral-culture and low-literacy communities — the seats bearing the heaviest access costs — hold no seat on translation-policy bodies, which are staffed by the credentialed. Members who left over access disputes rarely testify inside the tradition that shaped them.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight — translation norms flipping to communicative-effect primacy and language-training gates dissolving — the teaching office's necessity would be renegotiated within a generation, seminary curricula would reorganize around hermeneutics rather than philology, and doctrinal disputes would relocate from the text's wording to the translator's method. Conservative communities' authority architecture is load-bearing on this arrangement; removing it rearranges them.
% FOUNDING_PROBLEM: Securing the vernacular church's accountability to the source text: once scripture passed into vernacular hands, loose renderings enabled sectarian proof-texting and translator-inserted doctrine; the formal-equivalence settlement bound vernacular texts to source structure so that teaching could be checked against a fixed standard and translator discretion minimized.
% FOUNDING_PROBLEM_CORROBORATION: Outsiders corroborate the problem while disputing the solution: dynamic-equivalence theorists' entire methodological apparatus (explicit translation procedures, peer review of renderings) exists because translator discretion is real, and academic textual critics document that establishing the source text itself requires expert judgment. Neither party grants that the access-gating structure is the required remedy — both propose governing discretion without gating access — but both attest, from outside the benefiting parties, that the underlying problem is not solved.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70) because the arrangement prices direct access in years of language study or permanent dependence on mediation, and the specialist class's necessity is a product of the difficulty rather than incidental to it. Suppression is moderate (0.55): enforcement is real — official translation lists, seminary requirements, exclusivist translation movements, catechetical control — but readable translations circulate widely and exit to them is possible at social cost, so alternatives are narrowed rather than closed. Theater is moderate-low (0.30): philological work and teaching are functionally real, while a growing share of activity is ritual invocation of 'the Greek' or 'the Hebrew' in preaching with no translational consequence. Accessibility collapse sits at 0.60: granting the reading's premise (meaning is carried in source structure) collapses the lay self-access alternative almost completely, but the premise itself is contestable and readable translations persist as an outside option, keeping collapse short of natural-law levels. Resistance at 0.50 reflects a long record of vernacular and lay-access movements, met and contained but never extinguished. All three temporal series share one six-point grid (t=0..75); the suppression series is deliberately non-monotonic — enforcement ratcheted through the mid-century translation conflicts, then partially relaxed as plural conservative translations were licensed — modeling an enforcement ratchet followed by partial normalization rather than a static picture. Receipt surface: gain_flow is authored to teaching_office_clergy because deference, tuition, and interpretive status demonstrably accrue there; fixing_cost is prohibitive because the seats able to change translation policy would dismantle their own necessity by doing so, so the cost of fixing exceeds what they bear.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the difficulty is not a defect but the substance: fidelity that cost nothing would not be fidelity, and the teaching office experiences the arrangement as obedience to the text. From the payer seats the same structure is a wall with a tollgate — comprehension available only through years of study or through deference to those who studied. The engine computes these as different classifications from the same structural data. Among beneficiaries the experience also diverges: seminary faculty face a competitive training market with career capital sunk in the languages, while denominations collect cohesion and boundary-marking from textual stability and can afford patience. Coalition potential among payer seats is real and historically demonstrated — lay bible-society movements repeatedly forced wider access from below — which is why suppression is modeled as containment rather than closure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (teaching_office_clergy, seminary_biblical_language_faculty, hermeneutically_conservative_denominations) derive directionality near the beneficiary pole: the arrangement subsidizes their authority, income, and institutional identity, and their exits are costly in identity or career capital, deepening rather than damping the subsidy. Declared victims (untrained_lay_congregants, mission_field_convert_communities, oral_culture_low_literacy_communities) derive directionality near the target pole, amplified for the trapped seats — mission and oral-culture communities cannot leave the community that mediates the text, so they sit nearest full-target. Vindicated propositions (verbal_plenary_inspiration_doctrine, original_text_determinacy) are listed separately: the arrangement's operation vindicates them, but they collect no rents and feed no directionality. Dynamic-translation advocates are excluded rather than coordinated; their exclusion is the enforcement object itself and sits outside the derivation. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both mislabels. Reading the arrangement as pure extraction would predict that lifting enforcement collapses it — but the coordination function (one stable reference text limiting translator discretion, checkable teaching, preserved semantic density) would survive liberalization, as the voluntary persistence of essentially-literal translations demonstrates. Reading it as pure coordination would ignore the asymmetric access economics: the difficulty that serves fidelity also manufactures the specialist class's necessity, and enforcement machinery actively polices translation choice. Holding both: genuine coordination function, asymmetric extraction, active enforcement. On obsolescence: the founding problem (translator discretion threatening doctrinal drift) is corroborated as live by outsiders, so no zombie declaration fires; but the arrangement now performs more than its founding warrant — access-gating has accumulated beyond discretion-control — which is accumulation layered onto coordination, the signature the rising extractiveness series traces. The theater series tracks the complementary symptom: ritual invocation of the original languages growing faster than their operational use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formation_or_extraction,
    'Is the education burden placed on lay readers formation (this reading''s own framing: the discipleship cost of fidelity, willingly assumed) or extraction (rent collected by the teaching office, whose necessity the difficulty manufactures)?',
    'Correlate access with payment capacity across communities holding the same reading: if the burden persists where competent teaching is abundant and free, the formation account strengthens; if depth of access tracks ability to pay for education, the rent account strengthens.',
    'Formation-dominant resolution supports a coordination-heavy computation; extraction-dominant resolution pushes the arrangement toward the pure-extraction end and implicates the teaching office as capturing seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_or_extraction, empirical, 'Whether the access cost is legitimate pedagogy or rent, given that the reading itself endorses the burden.').

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates one reading (formal_equivalence_reading) of the kernel biblical_source_text; what would a sibling reading change structurally?',
    'Compare the three family stories'' victim sets and epsilon values: the dynamic_equivalence_reading relocates burdens onto doctrinal-stability interests and mission-field audiences of a different kind; the critical_reconstructive_reading relocates beneficiaries toward the academic guild that certifies the reconstructed text.',
    'The disagreement is located in the priority ordering between structural fidelity and communicative access, and consequently in who bears the cost of the gap between ancient languages and modern readers; resolving the kernel contest redistributes the victim set, not merely the label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexed beneficiary/victim sets across the biblical_source_text kernel family.').

omega_variable(
    source_text_determinacy_variants,
    'Does the source text exhibit the determinate structure the fidelity target presupposes, given documented variant readings (e.g., the longer ending of Mark, the Johannine comma) where the received structure differs across manuscript traditions?',
    'Measure how often variant readings change doctrine-affecting structure rather than trivia: survey confessional statements and teaching curricula for dependence on textually unstable passages.',
    'High doctrinal salience of variants destabilizes the fidelity target itself and shifts effective control to whichever seat certifies the critical text — converting a stability arrangement into a certification-dependency arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_text_determinacy_variants, empirical, 'Whether the arrangement''s warrant (a stable determinate source structure) survives the manuscript evidence.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression keeping lay readers dependent structural (cost and gating of language training, licensed-translation lists) or internalized (doctrines of clerical headship, the conviction that unaided reading is presumptuous)?',
    'Post-exit suppression trajectory: track members who move to communities using readable translations — if dependence on mediated reading persists after the structural gate is removed, a substantial internalized component is present.',
    'An internalized component raises effective suppression above the structural measure and means institutional reform alone would not release the payer seats; the suppression travels with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the enforcement that maintains mediated access.').

omega_variable(
    perspicuity_doctrine_tension,
    'Much of the host tradition also confesses the clarity (perspicuity) of scripture — how is ''the text is clear'' held alongside ''you need training to read it'', and does the tension resolve?',
    'Analyze catechetical and homiletic literature for how the two claims are reconciled, and observe lay-literacy revival movements, which recur precisely where the tension is felt most sharply.',
    'An unresolved tension predicts recurring lay-access revivals that erode the gate from inside the tradition; resolution in favor of clarity would collapse the access structure, resolution in favor of mediation would force abandonment of the clarity confession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_doctrine_tension, conceptual, 'Internal contradiction within the reading''s own tradition between textual clarity and teaching-mediated access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_source_text__formal_equivalence_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__formal_equivalence_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t45, biblical_source_text__formal_equivalence_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t45, observed).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__formal_equivalence_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t60, observed).
narrative_ontology:measurement(bibl_tr_t75, biblical_source_text__formal_equivalence_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_source_text__formal_equivalence_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__formal_equivalence_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t45, biblical_source_text__formal_equivalence_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement_basis(bibl_be_t45, observed).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__formal_equivalence_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(bibl_be_t60, observed).
narrative_ontology:measurement(bibl_be_t75, biblical_source_text__formal_equivalence_reading, base_extractiveness, 75, 0.7).
narrative_ontology:measurement_basis(bibl_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_source_text__formal_equivalence_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__formal_equivalence_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t45, biblical_source_text__formal_equivalence_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement_basis(bibl_su_t45, observed).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__formal_equivalence_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(bibl_su_t60, observed).
narrative_ontology:measurement(bibl_su_t75, biblical_source_text__formal_equivalence_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(bibl_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, information_standard).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'faithful Bible translation' covers three structurally distinct constraints (epsilon-invariance decomposition): this formal-equivalence reading (fidelity to source structure primary; access via community teaching), the dynamic_equivalence_reading (communicative effect primary; the translator bears intelligibility), and the critical_reconstructive_reading (recovery of the hypothetical original primary; the textual basis itself is the contested object). Each has its own epsilon, beneficiary set, and victims; they are linked here as a constraint family. Historical direction: the formal reading is upstream — its settlement established the text-standard the others react to; the critical reading exerts downstream pressure on both by changing what 'the source text' denotes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
