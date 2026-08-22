% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Domain-Bifurcated Latin Correctness Standard (Hybrid Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of the Latin correctness kernel is the compromise
 *   settlement: it does not demand full classical restoration everywhere (the
 *   rupture reading) nor treat medieval Latin's organic development as
 *   uniformly legitimate across all genres (the continuity reading). Instead
 *   it draws a domain line — literary and rhetorical composition must answer
 *   to classical models; technical, legal, medical, and administrative Latin
 *   may retain medieval forms without penalty. This looks like a reasonable
 *   division of labor, and in significant part it is: it lets practical Latin
 *   keep functioning while letting humanist literary culture have its
 *   exacting standard. But the line itself is drawn and enforced by the
 *   humanist institutions that benefit from the literary standard's prestige,
 *   and it produces a status hierarchy in which technical writers are told
 *   their register is 'fine for its purpose' while being denied the cultural
 *   capital attached to the classical register — and are sometimes pressured
 *   to import literary flourishes into technical prose to seek legitimacy
 *   they cannot structurally attain without abandoning technical precision.
 *
 * KEY AGENTS:
 *   - humanist_literary_scholars: agenda_setter/beneficiary — draw and police the domain boundary, certify literary correctness
 *   - rhetoric_instructors: beneficiary — teach and examine against the scarce classical standard
 *   - classical_philologists: beneficiary — produce the apparatus that defines classical correctness
 *   - technical_and_scientific_latin_writers: payer — functionally legitimate but status-subordinated, sometimes pressured toward an unreachable literary bar
 *   - vernacular_adjacent_notaries_and_clerks: payer — tolerated in practice, permanently excluded from prestige
 *   - medieval_scholastic_writers: excluded — fall into the framework's unaddressed middle zone
 *   - modern_historical_linguists: observer — study the settlement as a historical artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.48).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.42).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Domain-Bifurcated Latin Correctness Standard (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '6672dfe2-6b4a-44ab-9469-ed4bb17af8de').
narrative_ontology:cs_kernel_codification('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', distributed).
narrative_ontology:cs_authority_grounding('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', practice).
narrative_ontology:cs_interpretation_layer_present('6672dfe2-6b4a-44ab-9469-ed4bb17af8de').
narrative_ontology:cs_reading_relation('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', foundational, correctness_is_domain_relative).
narrative_ontology:cs_axiom_status(correctness_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', correctness_is_domain_relative, conventional).
narrative_ontology:cs_axiom('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', foundational, literary_register_requires_classical_fidelity).
narrative_ontology:cs_axiom_status(literary_register_requires_classical_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', literary_register_requires_classical_fidelity, conventional).
narrative_ontology:cs_reference_frame('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', ciceronian_augustan_literary_norm).
narrative_ontology:cs_drift_state('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', high_renaissance_humanism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6672dfe2-6b4a-44ab-9469-ed4bb17af8de', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_literary_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, rhetoric_instructors).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_philologists).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_and_scientific_latin_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, vernacular_adjacent_notaries_and_clerks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and police the classical standard for literary and oratorical Latin — editions, grammars, and pedagogical canons that credential 'correct' composition. They administer the bifurcation itself: literary Latin must answer to Cicero and Virgil, while they simultaneously grant technical Latin a separate, lower-status pass. Their prestige and teaching posts depend on the literary domain being judged by the harder standard they alone can certify.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_literary_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, humanist_literary_scholars, beneficiary).

% Teach and examine students in classical composition, deriving income and status from mastery of a standard that is scarce by design. The bifurcation protects their market: literary Latin's difficulty is the reason their instruction is valuable, while technical writing is left to lesser-paid scribes and notaries.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, rhetoric_instructors, beneficiary,
    organized, biographical, mobile, regional).

% Produce the critical apparatus — grammars, dictionaries, model editions — that defines what counts as classical usage in the literary domain. Their scholarly authority is constituted by the existence of a hard boundary between the domain they certify (literary/rhetorical) and the domain they exempt (technical/practical).
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_philologists, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Write medical, legal, and natural-philosophical treatises in medieval Latin forms that are functionally adequate for their domain, yet find their prose ranked as inherently inferior whenever it is read against literary criteria, or pressured to imitate an unreachable classical register to gain scholarly legitimacy. Cannot simply exit the Latin-writing world without losing access to the shared learned register; cannot fully meet the literary bar without abandoning the technical vocabulary their work requires.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_and_scientific_latin_writers, payer,
    moderate, biographical, constrained, regional).

% Produce administrative and legal Latin close to spoken/vernacular patterns, tolerated as legitimate within the hybrid framework's practical carve-out but permanently barred from the prestige register. Their documents are functionally accepted yet stylistically dismissed whenever compared to literary norms, and they have no institutional path to the higher-status domain.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_adjacent_notaries_and_clerks, payer,
    powerless, biographical, trapped, local).

% Wrote philosophical and theological Latin that is neither purely literary/rhetorical nor purely technical/practical in the hybrid framework's terms, and so falls into a contested middle zone the bifurcation does not clearly adjudicate. They would object that the two-domain split misdescribes their own genre but have no seat in the humanist institutions that set the boundary.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_scholastic_writers, excluded,
    moderate, generational, constrained, continental).

% Study the correctness debate itself as a historical object, documenting how the domain-bifurcation compromise emerged as an uneasy settlement between purist and continuity positions, and tracing which genres of medieval writing were absorbed, tolerated, or stigmatized under it.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, humanist_literary_scholars).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working settlement that avoids requiring every Latin-using genre to meet the full classical standard — technical and administrative writing is permitted to retain medieval forms so that law, medicine, and record-keeping can continue functioning without waiting on humanist certification, while literary and rhetorical production is held to a single legible prestige standard that coordinates elite education and patronage.
% TRANSFER_FUNCTION: Moves prestige, teaching income, and canonical authority toward those who can certify and perform classical literary Latin, while technical and practical writers retain functional legitimacy but are denied the status and patronage that attaches to the literary register — a status transfer more than a material one, though it shapes who gets teaching posts, commissions, and scholarly citation.
% ABSENT_VOICES: Medieval scholastic and philosophical writers whose genre sits between the two domains are not consulted in how the boundary is drawn; vernacular-adjacent notaries have no representation in the humanist institutions setting the literary standard and can only be spoken about, not to.
% DISAPPEARANCE_RATIONALE: If the bifurcated standard vanished, literary Latin instruction would lose its exclusive claim to 'correctness,' technical writers would no longer be measured against an inapplicable classical bar, and the status hierarchy privileging literary humanists over technical and administrative writers would have to be re-derived from some other criterion or collapse into a flatter field of Latin registers.
% FOUNDING_PROBLEM: Renaissance humanism needed a way to elevate classical literary style as the register of elite culture and education without simultaneously declaring centuries of functioning medieval legal, medical, and administrative Latin illegitimate and unusable.
% FOUNDING_PROBLEM_CORROBORATION: Humanist pedagogical institutions and their modern literary-historical descendants attest the domain split as a sensible division of labor. Historians of medieval science and administrative history, writing from outside the humanist tradition that benefits from the split, attest that the same 'merely technical' Latin was frequently linguistically sophisticated and that the bifurcation encodes a status judgment rather than a purely functional one.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) rather than high because the hybrid reading genuinely relieves technical writers of an impossible full-classical burden — most of the time their medieval forms are accepted at face value. The extraction is the status differential layered on top: literary correctness confers prestige and patronage that technical adequacy does not, and that differential is actively maintained (requires_active_enforcement) by humanist institutions whose authority depends on the split persisting. Suppression (0.42) reflects that technical writers are not coerced out of their forms but are structurally denied equal standing — a softer, status-based suppression rather than exclusion from practice. Accessibility collapse is moderate (0.4): technical writers retain a working alternative (their own domain's forms are legitimate), which is exactly why this reading is less extractive than the rupture reading would be.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars, rhetoric instructors, and philologists are declared beneficiaries because the bifurcation is the institutional structure through which their prestige and market position are produced and protected — d sits near the beneficiary end. Technical and scientific writers are victims of the status transfer, not of exclusion from practice — their exit options are constrained (they can write competent technical Latin freely but cannot buy their way into the higher register without genre-inappropriate imitation), giving them elevated but not maximal d. Notaries and clerks are declared trapped: local, powerless, and structurally barred from ever crossing into the prestige domain regardless of skill, which pushes their d toward the target end more strongly than the technical writers' constrained position.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is not simple extraction: it solves a real problem (letting practical Latin function without waiting on classical purism) and is therefore correctly read as tangled_rope rather than snare. Reading it as pure extraction would erase the genuine coordination benefit to technical writers of NOT being held to the rupture reading's impossible bar. Reading it as pure coordination (rope) would erase the status transfer that the domain line manufactures and that humanist institutions have every incentive to maintain even after any purely functional justification for it has weakened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_placement_ambiguity,
    'Who gets to decide which genres count as ''literary/rhetorical'' versus ''technical/practical,'' and is that boundary itself contested or self-evident?',
    'Trace historical cases at the boundary (scholastic philosophy, scientific poetry, humanist letters on practical topics) to see whether the domain assignment was stable or was itself a site of ongoing dispute and re-litigation.',
    'If the boundary is frequently contested and re-drawn by the same humanist institutions that benefit from the literary side being narrow and prestigious, that strengthens the tangled_rope reading (the coordination function is real but the boundary is administered in the beneficiaries'' interest). If the boundary tracked stable, pre-existing genre distinctions, the hybrid reading looks closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_placement_ambiguity, conceptual, 'Whether the literary/technical domain line is a natural genre boundary or an administered, interest-serving construct.').

omega_variable(
    hybrid_vs_sibling_readings_which_is_true,
    'Is the hybrid reading''s domain bifurcation itself a defensible description of how correctness was actually judged historically, or is it a modern retrospective smoothing that imposes a tidy two-domain structure onto a messier, more continuous set of practices (as the continuity_reading would hold) or onto a more uniformly hierarchical one (as the rupture_reading would hold)?',
    'Comparative corpus analysis of medieval Latin across genres, checking whether contemporaries actually applied a bifurcated standard consistently, or whether correctness judgments varied continuously by author, region, and period without a clean domain split.',
    'If the bifurcation is a real historical pattern, the hybrid_reading is the structurally accurate account and the other two readings each capture only part of the picture. If it is a retrospective imposition, all three readings are partially artifacts of later historiography rather than descriptions of a single settled arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_sibling_readings_which_is_true, conceptual, 'Whether the two-domain structure this reading assumes was a real contemporary distinction or a later analytical overlay.').

omega_variable(
    status_harm_measurability,
    'Can the status harm to technical writers (denied prestige, not denied function) be measured with the same confidence as material extraction, or is it a softer, harder-to-verify cost?',
    'Examine patronage records, career trajectories, and citation patterns comparing technical Latin authors to literary Latin authors of comparable skill, to see whether the prestige gap translated into material disadvantage (funding, positions, influence).',
    'If status harm cashes out in material terms (fewer positions, less patronage, less durable reputation), the victim classification for technical writers is well-grounded. If the harm remains purely reputational with no material trace, the extractiveness score may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_harm_measurability, empirical, 'Whether the prestige asymmetry produced material, not just reputational, costs for technical writers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__hybrid_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(lati_tr_t160, latin_correctness__hybrid_reading, theater_ratio, 160, 0.24).
narrative_ontology:measurement(lati_tr_t240, latin_correctness__hybrid_reading, theater_ratio, 240, 0.27).
narrative_ontology:measurement(lati_tr_t320, latin_correctness__hybrid_reading, theater_ratio, 320, 0.29).
narrative_ontology:measurement(lati_tr_t400, latin_correctness__hybrid_reading, theater_ratio, 400, 0.3).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lati_be_t80, latin_correctness__hybrid_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(lati_be_t160, latin_correctness__hybrid_reading, base_extractiveness, 160, 0.42).
narrative_ontology:measurement(lati_be_t240, latin_correctness__hybrid_reading, base_extractiveness, 240, 0.46).
narrative_ontology:measurement(lati_be_t320, latin_correctness__hybrid_reading, base_extractiveness, 320, 0.47).
narrative_ontology:measurement(lati_be_t400, latin_correctness__hybrid_reading, base_extractiveness, 400, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lati_su_t80, latin_correctness__hybrid_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(lati_su_t160, latin_correctness__hybrid_reading, suppression_requirement, 160, 0.4).
narrative_ontology:measurement(lati_su_t240, latin_correctness__hybrid_reading, suppression_requirement, 240, 0.41).
narrative_ontology:measurement(lati_su_t320, latin_correctness__hybrid_reading, suppression_requirement, 320, 0.42).
narrative_ontology:measurement(lati_su_t400, latin_correctness__hybrid_reading, suppression_requirement, 400, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the latin_correctness kernel. continuity_reading treats medieval Latin as organically legitimate across all domains (low bifurcation, low status extraction). rupture_reading treats classical Latin as a fixed standard from which all medieval usage deviates (high extraction, near-total victim set). hybrid_reading (this story) occupies the structural middle: a genuine domain-limited coordination function plus a partial, status-based extraction confined to writers seeking cross-domain prestige. The three share no single ε — each is authored independently per the ε-invariance principle, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
