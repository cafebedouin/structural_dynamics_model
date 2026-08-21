% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Corporate Enclosure: Maximal Protection Reading
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'corporate enclosure' reading of the
 *   constitutional mandate for copyright, which interprets 'limited times' as
 *   permitting maximal extension short of explicit perpetuity. It views
 *   copyright primarily as a property right requiring maximal protection,
 *   leading to policies like extended terms, criminalization of
 *   circumvention, and restriction of fair use. This reading is championed by
 *   corporate incumbents who benefit from prolonged monopolies, often at the
 *   expense of derivative creators, educators, and the public domain.
 *
 * KEY AGENTS:
 *   - corporate_incumbents: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - derivative_creators: Primary target/payer (powerless/constrained)
 *   - educators: Target/payer (moderate/constrained)
 *   - archivists: Target/payer (moderate/constrained)
 *   - public_domain_advocates: Excluded voice (organized/constrained)
 *   - legislators: Agenda_setter (institutional/mobile)
 *   - judiciary: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.8).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.75).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Corporate Enclosure: Maximal Protection Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'f10d4c21-3e9a-4522-9ef2-7cb117bc722d').
narrative_ontology:cs_kernel_codification('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', fixed_text).
narrative_ontology:cs_authority_grounding('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', extraction).
narrative_ontology:cs_interpretation_layer_present('f10d4c21-3e9a-4522-9ef2-7cb117bc722d').
narrative_ontology:cs_reading_relation('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', foundational, copyright_is_absolute_property).
narrative_ontology:cs_axiom_status(copyright_is_absolute_property, holdable).
narrative_ontology:cs_axiom_grounding('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', copyright_is_absolute_property, deontological).
narrative_ontology:cs_axiom('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', foundational, limited_times_is_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_is_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', limited_times_is_maximal_extension, conventional).
narrative_ontology:cs_reference_frame('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', maximal_corporate_protection).
narrative_ontology:cs_drift_state('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f10d4c21-3e9a-4522-9ef2-7cb117bc722d', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major media and content corporations (e.g., Disney, RIAA, MPAA) that actively lobby for copyright term extensions, stricter enforcement, and criminalization of circumvention. They benefit directly from prolonged monopolies on their intellectual property, which allows them to control licensing and distribution for decades.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, musicians, writers, and other creators who wish to build upon existing works. They face high licensing fees, legal threats, or outright prohibition, limiting their ability to create new cultural works and contribute to a vibrant public sphere.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    powerless, biographical, constrained, national).

% Teachers, professors, and educational institutions who rely on access to copyrighted materials for teaching and scholarship. They are constrained by restrictive fair use interpretations and high costs, impacting pedagogical methods and access to knowledge.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    moderate, biographical, constrained, national).

% Librarians, archivists, and cultural heritage institutions tasked with preserving and providing access to cultural works. Extended copyright terms and anti-circumvention laws make it difficult to digitize, preserve, and make accessible works that would otherwise be in the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, constrained, national).

% Organizations and individuals who champion the public domain and argue for a balance between creator rights and public access. They are often marginalized in legislative debates dominated by corporate lobbying, despite representing broad public interest.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    organized, generational, constrained, global).

% Members of Congress who pass copyright legislation. They are subject to significant lobbying pressure from corporate incumbents, leading to repeated extensions of copyright terms and the strengthening of enforcement mechanisms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legislators, agenda_setter,
    institutional, generational, mobile, national).

% Federal courts that interpret copyright law and the 'limited times' clause of the Constitution. They often defer to legislative discretion in extending copyright terms, reinforcing the corporate enclosure reading through judicial precedent.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Incentivizes the creation and dissemination of original works by granting creators temporary exclusive rights, thereby coordinating cultural production and investment.
% TRANSFER_FUNCTION: Transfers significant economic value from users, derivative creators, educators, and the public domain to copyright holders, primarily large corporate incumbents, through extended monopolies and restricted access to cultural works.
% ABSENT_VOICES: The general public, future creators, and those who would benefit from a richer public domain are largely absent from the legislative and judicial processes that extend copyright terms and restrict fair use. Their interests are often represented by underfunded advocacy groups.
% DISAPPEARANCE_RATIONALE: If this reading of copyright vanished overnight, copyright terms would likely revert to much shorter periods, the public domain would expand rapidly, and the economic models of many content industries would be forced to adapt. This would lead to a significant reorganization of cultural production, distribution, and access, with a surge in derivative works and public access to older content.
% FOUNDING_PROBLEM: To promote the progress of science and useful arts by securing for limited times to authors and inventors the exclusive right to their respective writings and discoveries, balancing creator incentives with public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public interest groups, and historical analysis attest that the original intent was a balance with the public domain, which has been eroded by successive term extensions. Corporate incumbents, however, assert that maximal protection is necessary for continued innovation and economic stability, framing the problem as ongoing threats to creator revenue.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the substantial economic value transferred from the public and derivative creators to corporate copyright holders through extended monopolies. Suppression (0.75) is high due to active enforcement of anti-circumvention laws, restrictions on fair use, and the legal and financial barriers faced by those challenging the status quo. The theater ratio is low (0.1) because the enforcement mechanisms are highly functional in protecting corporate interests, not merely performative. The increasing extractiveness and suppression over the interval reflect the historical trend of copyright term extensions and the strengthening of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Corporate incumbents perceive this reading as a necessary and just protection of intellectual property, essential for incentivizing creativity and investment. Conversely, derivative creators, educators, and public domain advocates experience it as an extractive mechanism that stifles creativity, limits access to knowledge, and undermines the public good. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are clear beneficiaries (d=0.0-0.1) as they directly collect rents from extended monopolies and shape the legislative agenda. Derivative creators, educators, and archivists are targets (d=0.8-1.0) as they bear the costs of licensing, legal restrictions, and a shrinking public domain. Legislators and the judiciary, while institutional, are influenced by the agenda-setters, often reinforcing the extractive structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by continuously re-legitimizing and extending the 'limited times' mandate, ensuring the constraint's function (from the corporate perspective) remains 'live.' However, from the perspective of the original constitutional intent, the mandate has been stretched beyond its original purpose, creating a functional shift from public scaffold to corporate enclosure. The 'contested' status of the founding problem reflects this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the constitutional phrase ''limited times'' a strict boundary on copyright duration, or a flexible grant of legislative discretion?',
    'A definitive Supreme Court ruling that establishes a non-arbitrary upper limit for copyright terms, or a constitutional amendment clarifying the intent.',
    'If interpreted as a strict boundary, the constraint''s extractiveness would decrease significantly, and its classification might shift towards a Rope or Scaffold. If confirmed as flexible legislative discretion, the current extractive structure would be further legitimized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the constitutional limits of copyright duration.').

omega_variable(
    economic_incentive_efficacy,
    'Do extended copyright terms genuinely incentivize the creation of new works, or primarily serve to protect existing assets and generate rents for corporate incumbents?',
    'Comprehensive, independent economic studies analyzing the correlation between term extensions and new creative output, controlling for other market factors.',
    'Empirical evidence showing negligible incentive effect would undermine the primary justification for this reading, potentially leading to legislative reform and reduced extractiveness. Strong evidence of incentive effect would reinforce the current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_incentive_efficacy, empirical, 'Whether extended copyright terms effectively promote new creation.').

omega_variable(
    public_domain_value_quantification,
    'How can the social, cultural, and economic value lost by a shrinking public domain be quantitatively measured and balanced against private copyright interests?',
    'Development of robust methodologies for valuing public domain contributions (e.g., through derivative works, educational access, innovation spillover) and their integration into policy-making frameworks.',
    'A clear quantification of public domain value would provide a stronger counter-argument to term extensions, potentially shifting the policy balance towards shorter terms and greater public access, thereby reducing the constraint''s extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_domain_value_quantification, conceptual, 'Difficulty in quantifying the value of the public domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.05).
narrative_ontology:measurement(copy_tr_t1986, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1986, 0.07).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(copy_tr_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(copy_tr_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2026, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(copy_be_t1986, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1986, 0.65).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(copy_be_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2008, 0.75).
narrative_ontology:measurement(copy_be_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2018, 0.78).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2026, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.6).
narrative_ontology:measurement(copy_su_t1986, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1986, 0.65).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(copy_su_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(copy_su_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_erosion).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_restriction).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel, alongside 'public_scaffold_reading' and 'judicial_ambiguity_reading'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
