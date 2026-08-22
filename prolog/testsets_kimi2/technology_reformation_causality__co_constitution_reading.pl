% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Printing Press and Reformation Co-Constitution
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the co_constitution_reading of the
 *   technology_reformation_causality kernel, which contests how printing
 *   technology and the Protestant Reformation causally interacted. Under this
 *   reading, the press and reformist social actors co-evolved: the press
 *   enabled vernacular mass communication but did not determine religious
 *   outcomes, while reformers actively shaped print content, genres, and
 *   distribution networks. The technology functioned as a rope-like
 *   coordination mechanism (solving genuine information-dissemination
 *   problems across fragmented Europe), but reformers' alternatives to
 *   print-based mobilization atrophied into a piton-like inertial structure.
 *   The constraint's extractiveness derives from the interaction term:
 *   neither the press alone nor the reformers alone generated the historical
 *   lock-in, but their co-constitution collapsed manuscript and oral-aural
 *   alternatives, asymmetrically benefiting literate reformist networks at
 *   the expense of traditional ecclesiastical institutions and illiterate
 *   populations. The claimed type is tangled_rope because the arrangement
 *   exhibits both genuine coordination and asymmetric extraction requiring
 *   active social maintenance.
 *
 * KEY AGENTS:
 *   - reformist_theologians: Agenda-setter (powerful/identity_locked) â shaped print content and became structurally dependent on continued print production
 *   - print_network_operators: Agenda-setter/beneficiary (organized/constrained) â ran presses and profited from reformist controversy
 *   - vernacular_literate_public: Beneficiary (moderate/constrained) â gained access but were locked into literate textual religiosity
 *   - traditional_clergy: Payer (institutional/constrained) â lost information monopoly and were forced into defensive print competition
 *   - illiterate_rural_communities: Payer (powerless/trapped) â excluded from the textual public sphere as legitimate theology shifted to print
 *   - media_studies_historians: Observer (analytical/analytical) â retrospective analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.48).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.42).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Printing Press and Reformation Co-Constitution").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, 'f7c8992c-25a1-4cb0-acd9-ca6818818105').
narrative_ontology:cs_kernel_codification('f7c8992c-25a1-4cb0-acd9-ca6818818105', distributed).
narrative_ontology:cs_authority_grounding('f7c8992c-25a1-4cb0-acd9-ca6818818105', expertise).
narrative_ontology:cs_interpretation_layer_present('f7c8992c-25a1-4cb0-acd9-ca6818818105').
narrative_ontology:cs_reading_relation('f7c8992c-25a1-4cb0-acd9-ca6818818105', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7c8992c-25a1-4cb0-acd9-ca6818818105', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('f7c8992c-25a1-4cb0-acd9-ca6818818105', foundational, technology_has_causal_efficacy).
narrative_ontology:cs_axiom_status(technology_has_causal_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('f7c8992c-25a1-4cb0-acd9-ca6818818105', technology_has_causal_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('f7c8992c-25a1-4cb0-acd9-ca6818818105', foundational, bidirectional_causality).
narrative_ontology:cs_axiom_status(bidirectional_causality, holdable).
narrative_ontology:cs_axiom_grounding('f7c8992c-25a1-4cb0-acd9-ca6818818105', bidirectional_causality, empirically_contingent).
narrative_ontology:cs_reference_frame('f7c8992c-25a1-4cb0-acd9-ca6818818105', co_constitutive_equilibrium).
narrative_ontology:cs_drift_state('f7c8992c-25a1-4cb0-acd9-ca6818818105', post_reformation_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7c8992c-25a1-4cb0-acd9-ca6818818105', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformist_theologians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, print_network_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_literate_public).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, traditional_clergy).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, illiterate_rural_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shaped the theological content and genre of printed works; their authority grew through print dissemination but became dependent on continued print production to maintain their reformist networks against Catholic and imperial opposition.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformist_theologians, agenda_setter,
    powerful, biographical, identity_locked, continental).

% Owned and operated presses, chose which reformist works to print based on market demand and patronage; their economic survival became linked to the continuation of religious controversy and vernacular demand.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, print_network_operators, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, print_network_operators, beneficiary).

% Gained access to religious debate in their own languages, forming a new public sphere; their religious identity became tied to literacy and print consumption, marginalizing oral and aural alternatives they had previously relied upon.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_literate_public, beneficiary,
    moderate, biographical, constrained, national).

% Lost the monopoly on sacred text interpretation and the controlled dissemination of theology; were compelled to enter the print market defensively or lose their flocks to reformist pamphlets.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, traditional_clergy, payer,
    institutional, generational, constrained, continental).

% Remained outside the textual public sphere; their religious practices and epistemologies were progressively marginalized as legitimate theology shifted to print-vernacular formats they could not directly access.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, illiterate_rural_communities, payer,
    powerless, generational, trapped, regional).

% Analyze the co-constitution of technology and social actors from a retrospective analytical seat; they neither benefited nor paid within the historical constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, media_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press and reformist networks co-evolved to solve the coordination problem of disseminating vernacular religious critique across politically fragmented Europe, bypassing the Church's Latin manuscript monopoly.
% TRANSFER_FUNCTION: Moves religious authority, material resources, and cultural legitimacy from traditional ecclesiastical institutions and oral-aural practice into the hands of reformist print networks and literate vernacular publics.
% ABSENT_VOICES: Illiterate rural communities dependent on oral liturgy, women in conventual oral traditions, and manuscript-based monastic scholars were excluded from the co-constituted textual public sphere; their epistemologies have no seat at the print-reform nexus.
% DISAPPEARANCE_RATIONALE: Without the co-constitution of press and reformist agency, the Reformation would not have taken its specific textual-literate form; religious controversy would have remained confined to manuscript, Latin, and oral channels, and the European public sphere would not have reorganized around vernacular print.
% FOUNDING_PROBLEM: How to communicate religious reform across linguistic and political boundaries while evading centralized ecclesiastical control over sacred information.
% FOUNDING_PROBLEM_CORROBORATION: Book historians and media archaeologists from outside the reformist beneficiary network attest that the print-reform alliance solved genuine information-control problems. Contemporary Catholic polemicists and imperial censors, who were non-beneficiaries, corroborate that the problem was solved at their expense.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.48 because the co-constitution genuinely coordinated vernacular religious communication (lower bound) while also locking out oral and manuscript alternatives (upper bound). Suppression is 0.42 because the collapse of non-print channels was structural and economic rather than directly coercive, though Counter-Reforcement censorship later added direct coercion. Theater ratio 0.35 captures the performative maintenance of reformist identity through repetitive print citation and pamphlet warfare as the movement institutionalized. Accessibility collapse 0.60 reflects that manuscript and oral channels became progressively non-viable for authoritative theology but did not fully disappear. Resistance 0.50 accounts for sustained Catholic and imperial opposition. The measurement series use a single shared grid (0â70) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The reformist theologian seat experiences the constraint as coordination they authored and shaped; the vernacular literate public experiences a mix of gained access and imposed literacy conversion; the traditional clergy and illiterate rural seats experience the same arrangement as the collapse of their epistemic and institutional worlds. The engine computes these divergences from the structural data â the agenda-setter's low directionality contrasts with the powerless payer's high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist theologians and print network operators sit near the beneficiary end (low d) because they set the agenda and collected authority or profit from the arrangement. The vernacular literate public sits near symmetric (moderate d) because they gained genuine access but paid in the currency of literacy conversion and lost oral-aural alternatives. Traditional clergy and illiterate rural communities sit near the target end (high d) because they bore the costs of a collapsed manuscript monopoly and exclusion from the textual public sphere. No overrides are needed: the structural derivation from beneficiary-victim declarations plus exit options (identity_locked for reformers, trapped for illiterate communities) captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to disseminate religious reform across boundaries while evading Church information control â was genuinely solved by the press-reform alliance (status dead by the 1520s). The arrangement persisted beyond the death of its founding problem, but it is not a pure piton because the coordination function remained live (continued vernacular access, ongoing print-based debate). It is not a pure snare because the press's coordination role was structurally real, not a cover story. Tangled_rope captures the coexistence of live coordination and asymmetric extraction without collapsing into either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_asymmetry_ambiguity,
    'Does the co-constitution reading collapse into disguised technological determinism or disguised social constructivism under empirical pressure?',
    'Comparative case studies of non-European print cultures (e.g., East Asian woodblock printing) where comparable reformist movements did not emerge; if technology alone did not produce reform, bidirectional co-constitution is sustained.',
    'Would reclassify the constraint toward pure rope if technology is neutral, or toward higher-extraction tangled_rope if technology imposes unacknowledged form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_asymmetry_ambiguity, conceptual, 'Whether co-constitution is stable or collapses into unidirectional causation').

omega_variable(
    reformer_identity_lock,
    'To what extent were reformers'' alternatives to print-based mobilization atrophied by structural dependence versus freely chosen strategic alignment?',
    'Archival evidence of reformers'' pre-print strategies and their explicit evaluations of oral, visual, and manuscript alternatives.',
    'If identity-locked, the reformer seat computes as higher directionality (more target than beneficiary), increasing the constraint''s measured asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_identity_lock, empirical, 'Structural dependence vs free choice in reformer print adoption').

omega_variable(
    suppression_nature_ambiguity,
    'Is the collapse of non-print religious channels structural (enforced by law or institution) or emergent (driven by market preference and network effects)?',
    'Analysis of legal penalties on oral tradition versus economic data on manuscript production costs and print market share.',
    'If purely emergent, suppression is overstated and the constraint is less extractive; if structural, suppression accurately reflects active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_nature_ambiguity, conceptual, 'Structural vs emergent suppression of non-print channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_ref_coconst_tr_t0, technology_reformation_causality__co_constitution_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_ref_coconst_tr_t15, technology_reformation_causality__co_constitution_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(tech_ref_coconst_tr_t30, technology_reformation_causality__co_constitution_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(tech_ref_coconst_tr_t45, technology_reformation_causality__co_constitution_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(tech_ref_coconst_tr_t60, technology_reformation_causality__co_constitution_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(tech_ref_coconst_tr_t70, technology_reformation_causality__co_constitution_reading, theater_ratio, 70, 0.35).

% Extraction over time
narrative_ontology:measurement(tech_ref_coconst_be_t0, technology_reformation_causality__co_constitution_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tech_ref_coconst_be_t15, technology_reformation_causality__co_constitution_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(tech_ref_coconst_be_t30, technology_reformation_causality__co_constitution_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(tech_ref_coconst_be_t45, technology_reformation_causality__co_constitution_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement(tech_ref_coconst_be_t60, technology_reformation_causality__co_constitution_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(tech_ref_coconst_be_t70, technology_reformation_causality__co_constitution_reading, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tech_ref_coconst_su_t0, technology_reformation_causality__co_constitution_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(tech_ref_coconst_su_t15, technology_reformation_causality__co_constitution_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(tech_ref_coconst_su_t30, technology_reformation_causality__co_constitution_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(tech_ref_coconst_su_t45, technology_reformation_causality__co_constitution_reading, suppression_requirement, 45, 0.36).
narrative_ontology:measurement(tech_ref_coconst_su_t60, technology_reformation_causality__co_constitution_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(tech_ref_coconst_su_t70, technology_reformation_causality__co_constitution_reading, suppression_requirement, 70, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_reformation_causality kernel, which decomposes into three structurally distinct claims about historical causation. Each reading carries a distinct epsilon, beneficiary-victim structure, and classification. This reading models bidirectional co-constitution; the determinism reading models unidirectional technological causation; the agency reading models unidirectional social causation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
