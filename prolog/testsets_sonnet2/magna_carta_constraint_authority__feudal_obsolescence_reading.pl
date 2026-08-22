% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Feudal Obsolescence Reading of Magna Carta's Constraint Authority
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal-obsolescence reading of a contested
 *   kernel: what constraint authority, if any, Magna Carta exercises over
 *   modern sovereignty structures. Under this reading, Magna Carta is
 *   understood strictly as it was in 1215 — a negotiated settlement between
 *   King John and rebellious barons resolving specific feudal grievances
 *   (relief payments, wardship, forest law, arbitrary seizure of baronial
 *   property) — and any claim that it binds contemporary constitutional
 *   practice is treated as an anachronistic overreading. The reading's
 *   practical deployment, however, is not neutral historical scholarship: it
 *   is selectively invoked by executive and security institutions
 *   specifically in contexts where Magna Carta's due-process lineage
 *   (chapters 39-40, later absorbed into habeas corpus jurisprudence) would
 *   otherwise constrain them. This is why the reading computes as extractive
 *   rather than merely descriptive — the same historical observation (the
 *   feudal provisions are obsolete) is used to license a much broader claim
 *   (no provision has binding force) that primarily benefits institutions
 *   seeking expanded discretion. Sibling readings —
 *   living_constitutionalism_reading and parliamentary_sovereignty_reading —
 *   are NOT part of this story; they are separate constraints with their own
 *   ε values, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - modern_executive_authorities: primary agenda-setter and beneficiary (institutional/arbitrage) — deploys the reading to expand discretion
 *   - national_security_apparatus: secondary beneficiary (institutional/arbitrage) — benefits when due-process lineage is denied binding force
 *   - popular_constitutionalism_advocates: primary payer (organized/constrained) — loses a doctrinal anchor
 *   - civil_liberties_litigants: concrete payer (powerless/trapped) — loses available remedies at point of harm
 *   - constitutional_historians: analytical observer — assesses the historical record independent of either camp's interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.55).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Feudal Obsolescence Reading of Magna Carta's Constraint Authority").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '0e5c56c7-ab0f-43af-9e58-d20a5b77ac55').
narrative_ontology:cs_kernel_codification('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', fixed_text).
narrative_ontology:cs_authority_grounding('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', extraction).
narrative_ontology:cs_interpretation_layer_present('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55').
narrative_ontology:cs_reading_relation('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', foundational, textual_authority_requires_originating_consent_continuity).
narrative_ontology:cs_axiom_status(textual_authority_requires_originating_consent_continuity, holdable).
narrative_ontology:cs_axiom_grounding('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', textual_authority_requires_originating_consent_continuity, conventional).
narrative_ontology:cs_axiom('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', secondary, feudal_specificity_exhausts_document_scope).
narrative_ontology:cs_axiom_status(feudal_specificity_exhausts_document_scope, holdable).
narrative_ontology:cs_axiom_grounding('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', feudal_specificity_exhausts_document_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', baronial_settlement_1215_specificity).
narrative_ontology:cs_drift_state('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', contemporary_executive_discretion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e5c56c7-ab0f-43af-9e58-d20a5b77ac55', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_authorities).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, judicial_restraint_traditions).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, civil_liberties_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the feudal-obsolescence reading to argue that Magna Carta's restraints on arbitrary detention and due process were bargained by barons against a specific 13th century king, not a perpetual charter binding contemporary sovereignty. Uses this argument to expand executive discretion in areas like emergency detention, prerogative powers, and administrative action, treating the 800-year gap as dispositive against any inherited constraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, modern_executive_authorities, beneficiary).

% Benefits directly when courts and legislatures accept that Magna Carta imposes no binding modern obligation, since this removes a rhetorical and doctrinal anchor historically cited against indefinite detention, denial of habeas corpus, and executive overreach in security contexts.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Relies on Magna Carta's chapters 39 and 40 (due process, denial of justice) as a living symbolic and doctrinal touchstone for popular sovereignty and the rule of law. The feudal-obsolescence reading strips this touchstone of binding force, leaving advocates only a rhetorical, non-enforceable heritage claim rather than an argument courts must credit.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, civilizational, constrained, national).

% Judges and legal traditions that cite Magna Carta genealogically to restrain executive action (habeas corpus jurisprudence, due process doctrine) lose a foundational citation when the obsolescence reading prevails, narrowing the doctrinal toolkit available for constraining contemporary state power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, judicial_restraint_traditions, payer,
    moderate, generational, constrained, national).

% Individuals detained, surveilled, or subjected to executive action who would otherwise invoke Magna Carta-descended due process guarantees find that argument foreclosed when courts accept it has no binding force over modern structures, leaving them with fewer available legal remedies at the moment of concrete harm.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, civil_liberties_litigants, payer,
    powerless, immediate, trapped, national).

% Study the actual historical function of Magna Carta as a baronial settlement with barons, not commoners, as primary beneficiaries in 1215, and assess competing claims about how much of its normative content survives structurally versus rhetorically in later constitutional orders.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% The original 13th century parties to the charter, now historically absent from the debate; their actual grievances (feudal relief payments, wardship abuses, forest law) are frequently elided when the modern reading is deployed, since the debate is fought entirely over the charter's contemporary legal status rather than what the barons actually secured or for whom.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, medieval_barons_class, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine in the present tense: the reading's function is not to coordinate current parties around a shared problem but to foreclose a line of legal argument. To the extent it coordinates anything, it coordinates executive and security institutions around a shared interpretive posture that treats historical constitutional claims as non-binding absent explicit modern re-enactment.
% TRANSFER_FUNCTION: Moves interpretive leverage from litigants and courts (who could otherwise invoke Magna Carta's due process lineage) to executive and security institutions (who gain discretion once that lineage is denied binding force). No money changes hands directly; what transfers is argumentative and doctrinal capital.
% ABSENT_VOICES: The medieval barons themselves, whose actual 1215 grievances (feudal exactions, arbitrary justice by King John) are rarely discussed on their own terms — they are cited only instrumentally to close off modern claims. Contemporary detainees and civil liberties litigants are present in courtrooms but structurally cannot compel the reading to be reconsidered once judicial precedent adopts it.
% DISAPPEARANCE_RATIONALE: If the feudal-obsolescence reading vanished (i.e., if courts and legislatures uniformly rejected it in favor of a living-constitutionalism or parliamentary-inheritance reading), executive claims to unconstrained prerogative in due-process-adjacent areas would lose a supporting argument, though not their only one — other doctrines (national security deference, statutory delegation) could substitute. Advocates for the reading would say nothing changes because the charter genuinely lacks binding force regardless of doctrine; critics would say significant realignment toward more legible, precedent-bound restraint would follow.
% FOUNDING_PROBLEM: The reading was constructed to resolve a genuine historical-interpretive puzzle: a 1215 baronial charter addressing wardship, relief payments, forest law, and feudal incidents does not straightforwardly map onto 21st century constitutional structures, and courts need a principled account of what, if anything, survives.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside both the executive-benefiting and advocate-benefiting camps broadly corroborate that Magna Carta's specific feudal provisions (relief, wardship, forest law) are indeed obsolete and do not bind modern practice — this part of the founding problem is genuinely live and well-attested. However, the further step — that the ENTIRE document, including chapters 39-40's due process language later absorbed into common law lineage, carries no binding force — is corroborated primarily by parties who benefit from expanded executive discretion (executive authorities, security apparatus) and is disputed by independent legal historians who distinguish the obsolete feudal machinery from the surviving due-process principle.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising because the reading's practical function has shifted over the measured interval from genuine historical clarification (correctly noting the feudal provisions are dead letter) toward a broader doctrinal tool for foreclosing due-process claims generally — a scope creep from 'obsolete feudal machinery' to 'no binding force at all.' Theater ratio is authored high (0.62) and rising because much of the reading's contemporary invocation is performative: it is cited in briefs and opinions less to resolve genuine interpretive difficulty than to signal that a historical-lineage argument need not be engaged on the merits. Suppression is moderate (0.55) — this is not enforced by coercive machinery in the way a snare typically requires, but by selective doctrinal adoption: courts and legislatures that accept the reading foreclose an argument path without needing to actively suppress dissent, which is why the constraint sits closer to piton-with-extractive-drift than to a fully coercive snare, though the claimed_type of snare reflects the judgment that the obsolescence claim is deployed instrumentally to enable extraction of discretion rather than as neutral historiography.
 *
 * DIRECTIONALITY LOGIC:
 *   Modern executive authorities and the national security apparatus sit at the beneficiary end: they gain discretion precisely because the reading denies that any modern claim can be grounded in the charter. Civil liberties litigants sit at the extreme target end — trapped, immediate time horizon, and directly deprived of an argument at the moment they need it most. Popular constitutionalism advocates and judicial restraint traditions are targets at a longer time horizon: they lose doctrinal capital accumulated over centuries. The medieval barons themselves are excluded non-parties to the modern debate entirely — a structural irony, since the reading purports to honor historical specificity yet the actual historical barons' grievances are invoked only instrumentally.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem — genuine historical-interpretive difficulty in mapping feudal provisions onto modern law — is substantially resolved and uncontested as to feudal minutiae (relief, wardship, forest law). The mandatrophy risk is that this narrow, legitimate resolution is used to smuggle in resolution of the CONTESTED question (whether due-process lineage survives), which independent historians do not corroborate as equally settled. This is the mismatch the six_questions battery is designed to surface: founding_problem_status is 'contested' precisely because corroboration splits along beneficiary lines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_specificity_vs_general_denial_conflation,
    'Does the feudal-obsolescence reading''s valid observation (specific feudal provisions like relief and wardship are dead letter) license its further claim (no provision, including due-process language, carries binding force)?',
    'Doctrinal history tracing whether chapters 39-40 were treated as severable and independently transmitted through common law (habeas corpus, due process jurisprudence) distinct from the feudal machinery, versus courts'' actual citation practice over time.',
    'If severability is established, the obsolescence reading''s extension to due-process content is a non-sequitur deployed instrumentally — supporting the snare classification. If no genuine severability exists, the reading is more defensible as coherent historical reading rather than instrumentalized extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_specificity_vs_general_denial_conflation, conceptual, 'Whether obsolescence of feudal specifics licenses denial of all binding residue.').

omega_variable(
    kernel_framing_alternative_institutional_vs_legitimacy,
    'Is the more analytically productive framing of this kernel the institutional one (which body — judiciary, Parliament, none — inherits constraint authority) or the legitimacy one (whether ANY inherited textual claim can bind a sovereign that never consented to it)?',
    'Compare classification outcomes under each framing: the institutional framing (adopted here and in the sibling readings) treats this as a three-way contest among reading communities; a legitimacy framing would treat the deeper question of textual inheritance across centuries as the kernel, potentially producing a different reading taxonomy entirely (e.g., consent-based vs. tradition-based legitimacy, cutting across the three readings given here).',
    'Under the institutional framing (used here), this reading computes as snare/piton due to instrumentalized denial of due-process lineage. Under a legitimacy framing, the same reading might classify closer to a defensible mountain-adjacent claim (no ancient text can bind a non-consenting modern sovereign) with much lower extractiveness, since the beneficiary structure would look different (all modern institutions equally freed, not specifically executive/security actors).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_institutional_vs_legitimacy, conceptual, 'Alternative kernel framing (institutional inheritance vs. sovereign-consent legitimacy) would materially change classification.').

omega_variable(
    genuine_vs_instrumental_motive_ambiguity,
    'Is the feudal-obsolescence reading held by its proponents as a genuine historical-interpretive conviction, or is it adopted instrumentally because it serves executive/security interests, with historical rigor as post-hoc justification?',
    'Track whether proponents of the reading apply the same obsolescence logic consistently to OTHER ancient-lineage doctrines that would constrain executive power less conveniently, or whether the obsolescence argument is asymmetrically deployed only where it expands discretion.',
    'Consistent application would support a more good-faith, lower-extraction reading; asymmetric application would strongly corroborate the snare classification and the instrumentalization thesis in the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_instrumental_motive_ambiguity, empirical, 'Whether the reading is genuinely historically motivated or instrumentally deployed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(magn_tr_t8, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(magn_tr_t16, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(magn_tr_t24, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(magn_tr_t32, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(magn_be_t8, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(magn_be_t16, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(magn_be_t24, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(magn_be_t32, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(magn_su_t8, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(magn_su_t16, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(magn_su_t24, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(magn_su_t32, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(magn_su_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_constraint_authority kernel: feudal_obsolescence_reading (this story), living_constitutionalism_reading, and parliamentary_sovereignty_reading. Each reading has its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — the natural-language label 'Magna Carta's authority' covers three structurally distinct claims about what, if anything, survives and who administers it. This story's ε (0.68, snare-leaning) is markedly higher than the living-constitutionalism reading's likely ε (which would treat the same due-process lineage as a genuine, low-extraction Rope/Mountain-adjacent inheritance) because the two readings disagree about whether the due-process content is severable from the obsolete feudal machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
