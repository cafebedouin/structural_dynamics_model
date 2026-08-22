% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Collective Security (Militia-Conditioned) Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the collective-security reading of the Second
 *   Amendment kernel: the prefatory militia clause is treated as operative,
 *   conditioning the right to keep and bear arms on connection to organized,
 *   state-regulated civic defense. Under this reading, the state's power to
 *   license, register, and restrict individual possession is constitutionally
 *   legitimate because the amendment's core purpose is maintaining a
 *   well-regulated militia, not protecting an unconditional personal right.
 *   This is a distinct constraint from the individual_right_reading (which
 *   treats the operative clause as self-standing) and from the
 *   originalist_civic_virtue_reading (which reads 'militia' as the universal
 *   armed citizenry rather than an organized state body) — the three share a
 *   text but instantiate structurally different constraints with different
 *   beneficiaries, different victim classes, and different epsilon values,
 *   per the kernel decomposition rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.38).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Collective Security (Militia-Conditioned) Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'c438b8dd-ae3f-4101-b9e3-19872b2dcf82').
narrative_ontology:cs_kernel_codification('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', fixed_text).
narrative_ontology:cs_authority_grounding('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', lineage).
narrative_ontology:cs_interpretation_layer_present('c438b8dd-ae3f-4101-b9e3-19872b2dcf82').
narrative_ontology:cs_reading_relation('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', foundational, militia_clause_is_substantive_condition).
narrative_ontology:cs_axiom_status(militia_clause_is_substantive_condition, holdable).
narrative_ontology:cs_axiom_grounding('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', militia_clause_is_substantive_condition, conventional).
narrative_ontology:cs_axiom('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', secondary, state_organized_defense_body_is_constitutional_referent).
narrative_ontology:cs_axiom_status(state_organized_defense_body_is_constitutional_referent, holdable).
narrative_ontology:cs_axiom_grounding('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', state_organized_defense_body_is_constitutional_referent, empirically_contingent).
narrative_ontology:cs_reference_frame('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', militia_conditioned_collective_defense).
narrative_ontology:cs_drift_state('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', post_national_guard_federalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c438b8dd-ae3f-4101-b9e3-19872b2dcf82', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_successor_institutions).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_interest_groups).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_retailers).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, rural_self_defense_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and licensing agencies design permit regimes, waiting periods, registration requirements, and prohibited-person categories, justified as necessary to a well-regulated collective defense capacity. It administers the licensing infrastructure, collects fees, and gains discretionary authority over who may lawfully possess arms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, state_regulatory_apparatus, beneficiary).

% The National Guard and analogous state-organized bodies are treated, under this reading, as the constitutionally contemplated militia. Their institutional monopoly on organized armed service is reinforced when courts read the clause as conditioning the right on collective military-adjacent organization rather than private possession.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, organized_militia_successor_institutions, beneficiary,
    institutional, generational, analytical, national).

% Gun-violence-prevention organizations and allied public health researchers gain a doctrinal foothold: if the right is conditioned on militia service, broad regulation of private ownership becomes constitutionally permissible, advancing their policy goals without a constitutional amendment.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Private citizens seeking to own firearms for self-defense, hunting, or sport find their claim to a personal right subordinated to a showing of connection to organized civic defense that, for most people, does not exist. They must navigate licensing regimes whose constitutional legitimacy under this reading is largely unreviewable at the individual level; leaving the jurisdiction to escape restrictive regimes is possible but costly.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    powerless, biographical, constrained, national).

% Dealers and manufacturers face compliance costs and demand suppression tied to licensing thresholds justified by the collective-security framing. Their market access is contingent on regulatory permission rather than an individual right presumptively protecting sale to qualified buyers.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_retailers, payer,
    moderate, biographical, constrained, regional).

% Residents in areas with slow law-enforcement response who rely on personal firearms for practical self-defense find that this reading treats their claim as constitutionally marginal absent militia affiliation, leaving their interest dependent on statutory grace rather than constitutional guarantee.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, rural_self_defense_claimants, payer,
    powerless, biographical, trapped, local).

% Scholars advancing the individual-right and civic-virtue readings are not incorporated into this reading's framework; their historical evidence about founding-era universal militia obligation is treated as supporting collective organization rather than individual entitlement, and their objections surface mainly in dissenting opinions and law review responses.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, originalist_legal_scholars, excluded,
    organized, generational, analytical, national).

% Courts adjudicate between this reading and its rivals, deciding which historical and textual evidence controls. Their choice of reading determines which stakeholder class bears the constraint's costs going forward.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reading the militia clause as operative language solves the problem of maintaining an organized, accountable collective defense capacity without ceding arms proliferation entirely to unregulated private discretion — it ties the constitutional guarantee to a structure (organized militia service) that can be trained, disciplined, and called to public service.
% TRANSFER_FUNCTION: Moves the presumption of legitimacy in arms regulation from the private citizen to the state: where courts adopt this reading, the burden shifts so that individual possession claims must be justified against a collective-security backdrop, and regulatory authority (and its associated fees, discretion, and enforcement power) flows to state licensing agencies.
% ABSENT_VOICES: Individual gun owners without militia affiliation, and the originalist scholars who read founding-era militia as coextensive with the armed citizenry, are structurally sidelined by this reading's framework — their objections appear as dissents and academic critique rather than as controlling doctrine within this reading.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned by every court and legislature currently relying on it, the doctrinal basis for a wide range of licensing and possession restrictions would evaporate; state regulatory apparatuses would lose the constitutional cover they use to condition possession on organized civic-defense connection, and individual-right claimants would gain a materially stronger presumptive claim.
% FOUNDING_PROBLEM: The founding-era anxiety was that a standing professional army under central control could threaten liberty, while a wholly unorganized armed populace could not provide reliable collective defense; the militia clause was meant to preserve state-organized, trained bodies of citizen-soldiers as a check on both dangers.
% FOUNDING_PROBLEM_CORROBORATION: State attorneys general and public-safety scholars attest the militia-linkage problem remains live in modified form (state control over organized force, accountability of armed actors). Independent historians of the founding era, writing outside both the gun-control and gun-rights advocacy communities, are divided: some corroborate the collective-organization reading of 'militia,' others find founding-era usage treated the militia as effectively the whole armed populace, undercutting the claim that the founding problem maps cleanly onto this reading's modern licensing apparatus.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42: real coordination value exists (organized, accountable, trainable defense capacity; reduced diffusion of ungoverned lethal force) but the cost falls asymmetrically on individual owners whose personal-security or recreational interest is treated as constitutionally marginal. Suppression (0.38) is lower than extraction because much of the doctrinal effect operates through court decisions and legislative discretion rather than direct physical coercion against owners, though licensing denial and prosecution for unlicensed possession are real enforcement mechanisms. Theater ratio is low (0.20) because the regulatory apparatus this reading legitimizes does perform substantive vetting functions, not merely symbolic ones. Resistance is high (0.72) reflecting the genuinely contested, high-salience nature of the doctrinal dispute — this is not a settled constraint but one actively fought over in courts, legislatures, and political discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory apparatus and successor militia institutions sit near the beneficiary end: they gain expanded, court-legitimated authority to condition, license, and restrict. Public safety interest groups benefit indirectly by gaining doctrinal cover for policy goals. Individual gun owners, firearms retailers, and rural self-defense claimants sit near the target end: their claims are subordinated to a collective-organization showing most private citizens cannot make, and their exit options (relocating to jurisdictions with different interpretive postures, or lobbying for doctrinal change) are real but costly and slow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fear of both an unchecked standing army and an unreliable, undisciplined armed populace — persists in modified form (state accountability over organized force remains a live concern) but the specific institutional form (state militias as the primary organized defense body) has been substantially superseded by professional standing armed forces and the National Guard's federalized function, which is structurally different from the founding-era state militia. This reading's continued doctrinal weight despite this institutional shift is exactly the founding_problem_status: contested signal — corroboration is split between those who see continuity of purpose and those who see doctrinal inertia riding on an obsolete institutional referent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_operative_or_prefatory,
    'Is the militia clause a substantive condition on the right (this reading), or purely explanatory/prefatory language that does not limit the operative clause''s independent guarantee (individual_right_reading)?',
    'No empirical resolution is available; this is a question of interpretive method (textualism, structuralism, purposivism) resolved by which controlling precedent a jurisdiction follows. Comparative doctrinal analysis across state constitutions with differently worded arms provisions can inform but not settle it.',
    'If the prefatory reading controls (as in the individual_right_reading), this constraint''s entire beneficiary/victim structure inverts: the state regulatory apparatus loses its constitutional warrant and individual owners become the protected class rather than the constrained one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_clause_operative_or_prefatory, conceptual, 'Core interpretive dispute over whether the militia clause is legally operative or merely explanatory.').

omega_variable(
    founding_era_militia_referent,
    'Did founding-era usage of ''militia'' refer to an organized, state-controlled body (supporting this reading) or to the armed citizenry generally (supporting originalist_civic_virtue_reading)?',
    'Historical linguistic and legal-corpus analysis of founding-era militia statutes, correspondence, and ratification debates; this is contested among historians and the evidence supports both readings to differing degrees depending on jurisdiction and period examined.',
    'If the armed-citizenry referent is historically correct, this reading''s claim to originalist legitimacy weakens substantially, though it could still be defended on living-constitutionalism or structural grounds independent of original meaning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_era_militia_referent, empirical, 'Historical dispute over the founding-era meaning of ''militia'' underlying the collective-security reading''s originalist claims.').

omega_variable(
    modern_militia_institutional_gap,
    'Does the National Guard''s federalized, professionalized character sever the constitutional continuity this reading relies on, such that the ''well-regulated militia'' this reading defends no longer structurally exists?',
    'Institutional history of the Militia Acts and National Defense Act reorganizations; comparison of state militia call-up authority historically versus National Guard federalization procedures today.',
    'If the institutional referent has genuinely disappeared, this reading''s founding-problem corroboration weakens and the constraint drifts toward zombie/mandatrophy status — persisting doctrinally while its institutional anchor has dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_militia_institutional_gap, empirical, 'Whether the organized-militia institution this reading references still exists in a form continuous with the founding-era referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__collective_security_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__collective_security_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__collective_security_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.17).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__collective_security_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__collective_security_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__collective_security_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__collective_security_reading, base_extractiveness, 1939, 0.28).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.34).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__collective_security_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__collective_security_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__collective_security_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_text__collective_security_reading, suppression_requirement, 1939, 0.22).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.33).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__collective_security_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the second_amendment_text kernel. Each reading is a structurally distinct constraint with its own epsilon, beneficiary/victim set, and classification: collective_security_reading (this story, tangled_rope — genuine coordination function in organized defense accountability, but asymmetric extraction from unaffiliated individual owners); individual_right_reading (expected rope or snare depending on regulatory intensity, with state licensing regimes as the extractive element instead); originalist_civic_virtue_reading (expected mountain-adjacent or rope, grounding the right in a universal citizen-soldier obligation that treats broad ownership as the coordination function itself). All three link bidirectionally via affects_constraints since a shift in any one reading's doctrinal dominance restructures the practical operation of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
