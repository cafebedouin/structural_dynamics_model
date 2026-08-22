% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe-Memory Mourning Ritual as Boundary-Enforcement Mechanism
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint isolates the boundary-maintenance function of a shared
 *   catastrophe-mourning ritual — the way its shared, mandatory form marks
 *   who is inside the group and who is not, and enforces conformity through
 *   social sanction. As external persecution pressure has receded over the
 *   measured interval, the practice's enforcement machinery has not relaxed
 *   at the same rate; theater ratio (performative boundary-marking distinct
 *   from grief-processing function) has risen while raw suppression intensity
 *   has eased slightly as younger members increasingly comply out of
 *   habituated identity-fusion rather than active coercion. This is a
 *   boundary-maintenance reading only — sibling readings of the same
 *   underlying ritual (survival-competence transmission, symbolic continuity,
 *   trauma encoding) are separate constraints with separate ε values and
 *   separate beneficiary/victim structures; this story does not average
 *   across them.
 *
 * KEY AGENTS:
 *   - communal_leadership: sets and enforces the sanctioned form (institutional/arbitrage) — collects communal authority
 *   - in_group_cohesion_stakeholders: benefit from strong shared identity and mutual aid (organized/constrained)
 *   - dissenting_members: bear social sanction for deviation (powerless/trapped)
 *   - intermarried_families: bear conditional-belonging scrutiny (powerless/constrained)
 *   - out_group_relations: excluded from joint memory-work, bear reinforced separateness (moderate/constrained)
 *   - younger_generation_members: identity-fused participants, both beneficiary and payer (powerless/identity_locked)
 *   - comparative_ritual_scholars: analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe-Memory Mourning Ritual as Boundary-Enforcement Mechanism").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'b586d59c-7402-46e1-bc04-bc344a17f6d3').
narrative_ontology:cs_kernel_codification('b586d59c-7402-46e1-bc04-bc344a17f6d3', implicit).
narrative_ontology:cs_authority_grounding('b586d59c-7402-46e1-bc04-bc344a17f6d3', practice).
narrative_ontology:cs_interpretation_layer_present('b586d59c-7402-46e1-bc04-bc344a17f6d3').
narrative_ontology:cs_reading_relation('b586d59c-7402-46e1-bc04-bc344a17f6d3', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b586d59c-7402-46e1-bc04-bc344a17f6d3', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('b586d59c-7402-46e1-bc04-bc344a17f6d3', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('b586d59c-7402-46e1-bc04-bc344a17f6d3', foundational, group_boundary_integrity_requires_costly_signal).
narrative_ontology:cs_axiom_status(group_boundary_integrity_requires_costly_signal, holdable).
narrative_ontology:cs_axiom_grounding('b586d59c-7402-46e1-bc04-bc344a17f6d3', group_boundary_integrity_requires_costly_signal, instrumental).
narrative_ontology:cs_axiom('b586d59c-7402-46e1-bc04-bc344a17f6d3', secondary, individual_conformity_subordinate_to_collective_distinctiveness).
narrative_ontology:cs_axiom_status(individual_conformity_subordinate_to_collective_distinctiveness, holdable).
narrative_ontology:cs_axiom_grounding('b586d59c-7402-46e1-bc04-bc344a17f6d3', individual_conformity_subordinate_to_collective_distinctiveness, conventional).
narrative_ontology:cs_reference_frame('b586d59c-7402-46e1-bc04-bc344a17f6d3', post_catastrophe_founding_solidarity).
narrative_ontology:cs_drift_state('b586d59c-7402-46e1-bc04-bc344a17f6d3', contemporary_low_persecution_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b586d59c-7402-46e1-bc04-bc344a17f6d3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, dissenting_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, younger_generation_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, younger_generation_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the calendar and correct form of the mourning observance, determines who participates in good standing, and adjudicates deviations. Draws legitimacy and communal authority from being custodian of the correct practice; can modify or waive elements of the ritual for favored members while enforcing strict compliance on others.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% The core community that participates fully and willingly gains a strong, legible sense of shared identity, mutual aid networks, and continuity with ancestors through the shared performance of mourning. Their solidarity is real and self-reported as valuable, but it is produced partly through the same mechanism that excludes and pressures others.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders, beneficiary,
    organized, generational, constrained, national).

% Members who question the observance's mandatory form, want to mourn differently, or want to skip it face social sanction: exclusion from communal roles, gossip, loss of standing for their children in matchmaking or schooling networks. Exit means losing family and community ties built over a lifetime, not merely opting out of a practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, dissenting_members, payer,
    powerless, biographical, trapped, local).

% Families where one partner is from outside the group face pressure to conform the household's mourning practice to the in-group standard or risk being treated as diluting the boundary. Their children's belonging is treated as conditional on compliance, and they bear ongoing scrutiny that born-in members do not.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_families, payer,
    powerless, biographical, constrained, local).

% Neighboring communities and individuals who might otherwise participate in shared civic mourning, interfaith commemoration, or joint memory-work find the ritual's boundary-marking function actively discourages joint participation, reinforcing separateness and periodically fueling mutual suspicion or resentment across group lines.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations, payer,
    moderate, biographical, constrained, regional).

% Raised inside the practice from childhood, they receive real belonging and inherited meaning from participation, but many report the observance functioning less as chosen meaning-making and more as an identity test they cannot safely fail without losing family standing — their attachment to the practice is fused with their sense of self before they are old enough to evaluate it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, younger_generation_members, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, younger_generation_members, beneficiary).

% Study the mourning-practice comparatively across communities and periods, documenting where the boundary-maintenance function is strong versus weak, and how it correlates with historical persecution intensity and current levels of external threat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the community with a shared, legible marker of who belongs, synchronizing collective memory of catastrophe into a coordinated calendar that lets members recognize one another as co-members and mobilize mutual aid and continuity across generations.
% TRANSFER_FUNCTION: Moves social standing, marriage eligibility, and communal belonging from those who deviate or are ambiguous in their compliance toward those who perform the practice in its sanctioned form; moves the cost of maintaining group distinctiveness onto individuals with mixed loyalties, doubts, or outside ties.
% ABSENT_VOICES: Dissenting members who want to mourn differently rarely have a forum to say so without incurring the very sanction the practice threatens; intermarried families and their children are discussed as a 'problem' by leadership rather than consulted as parties with a stake in how boundaries are drawn; out-group neighbors have no standing in the ritual's design process at all.
% DISAPPEARANCE_RATIONALE: Communal leadership and core cohesion beneficiaries would say the community's distinct identity would dissolve into the surrounding population within a generation or two — the world genuinely rearranges for them. Dissenting members and intermarried families would say daily life would improve immediately, with lifted social sanction and freer intermarriage, and the underlying grief and memory could still be marked in less boundary-policing ways. Comparative scholars note the coordination function (memory transmission) could survive the practice's disappearance if boundary-enforcement were the only thing removed.
% FOUNDING_PROBLEM: In the aftermath of catastrophic persecution or destruction, the community needed a way to hold itself together as a distinct, mutually recognizable group capable of collective action, given the alternative was assimilation and dispersal that would have erased the group as an entity.
% FOUNDING_PROBLEM_CORROBORATION: Communal leadership and elder members attest the assimilation threat remains live in the present, citing intermarriage rates and demographic decline. Comparative ritual scholars and sociologists studying diaspora communities, working from outside the benefiting leadership structure, note that boundary-maintenance intensity in many communities has grown even as external persecution risk has fallen, suggesting the founding problem (survival against active persecution) has partly receded while the boundary-enforcement apparatus built to serve it has not correspondingly relaxed.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the boundary-maintenance function genuinely produces real coordination value (mutual aid, legible belonging) for the in-group even as it imposes real, asymmetric costs on dissenters, intermarried families, and out-group relations through the same mechanism — this is the hybrid signature of tangled_rope, not pure extraction. Suppression is authored as a raw structural property (0.48, essentially flat to slightly declining) reflecting that overt coercive sanction has eased somewhat even as extraction crept upward, which is why suppression is NOT scaled alongside extractiveness in this story — they move independently, as the framework requires. Theater ratio rises across the interval (0.10 to 0.28) as the boundary-marking function increasingly persists through habit and performance rather than active persecution-driven necessity.
 *
 * PERSPECTIVAL GAP:
 *   From communal leadership's seat, the practice is coordination they administer in service of survival; from dissenting members' and intermarried families' seats, the identical structure computes as enforced conformity with real exit costs. The engine should compute a Tangled Rope from leadership's and cohesion-beneficiaries' structural position and something closer to Snare from the trapped, powerless payer seats — that divergence is the intended output, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal leadership sits near the full-beneficiary end: institutional power, arbitrage exit, collects communal authority. In-group cohesion beneficiaries sit moderately toward the beneficiary end but retain some cost (constrained exit, some conformity pressure even for compliant members). Dissenting members and intermarried families sit near the full-target end: powerless, trapped or constrained exit, bear the sanction the practice's enforcement exists to apply. Younger-generation members are the most structurally interesting seat: identity_locked exit reflects that their attachment to the practice is fused with self-concept formed before capacity for independent evaluation, which the automatic derivation captures reasonably well without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is deliberately contested rather than resolved: leadership's status=live claim (persecution/assimilation threat persists) is corroborated only by insiders, while comparative-scholar observation from outside the beneficiary set documents boundary-enforcement intensity growing even as external threat recedes — the classic zombie-mandate signature. Tangled_rope (rather than snare) is the correct call because a genuine coordination function (legible group continuity, mutual aid networks) persists alongside the extraction; collapsing it to pure extraction would erase the real value in-group beneficiaries report, and collapsing it to pure rope would erase the real, asymmetric costs borne by dissenters and out-group relations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_function_vs_memory_function_separability,
    'Is the boundary-enforcement function structurally inseparable from the ritual''s memory-transmission function, or could the community preserve catastrophe-memory through less exclusionary practice?',
    'Comparative study of diaspora communities that have relaxed mandatory conformity in mourning observance while retaining commemorative practice: if collective memory and mutual aid persist without the exclusion mechanism, the functions are separable.',
    'If separable, the boundary-enforcement costs (exclusion, conformity pressure) are pure extraction riding on a genuine memory-preservation function; if inseparable, some measured extraction is the unavoidable price of maintaining group distinctiveness at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_function_vs_memory_function_separability, conceptual, 'Whether boundary-maintenance and memory-transmission are separable functions of the same ritual.').

omega_variable(
    assimilation_threat_currency,
    'Is the assimilation/dissolution threat that justifies strict boundary enforcement still empirically live at the intensity leadership claims, or has it substantially receded since the ritual''s founding conditions?',
    'Demographic and sociological tracking of intermarriage rates, communal retention, and external persecution indicators over multiple generations, cross-checked against leadership''s public justifications for enforcement intensity.',
    'If the threat has substantially receded, the founding_problem_status shifts from contested toward dead, strengthening the zombie-mandate reading; if the threat remains materially live, the enforcement intensity is closer to proportionate response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_threat_currency, empirical, 'Whether the assimilation threat justifying boundary enforcement remains empirically live.').

omega_variable(
    framing_choice_boundary_vs_kernel_holistic,
    'Is decomposing the catastrophe-memory kernel into four separate readings (this one plus symbol-continuity, survival-competence, trauma-encoding) the right unit of analysis, or does treating the ritual as one irreducibly multi-functional practice better capture how practitioners themselves experience it?',
    'Ethnographic interview data asking practitioners whether they experience the mourning-practice as functionally decomposable (i.e. they could articulate ''this part is about belonging, this part is about memory'') or as a single undifferentiated meaningful whole.',
    'If practitioners experience it as irreducibly whole, the four-reading decomposition may be an analytical artifact that obscures how the functions reinforce one another in lived practice, even though ε-invariance requires the decomposition for classification purposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_boundary_vs_kernel_holistic, conceptual, 'Whether the four-reading kernel decomposition matches practitioners'' own experience of the practice as one undifferentiated whole.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the catastrophe_memory_kernel per the ε-invariance principle. Each sibling reading (symbol_continuity, survival_competence, trauma_encoding) treats a different observable of the same underlying mourning-practice as its referent and computes a distinct ε and beneficiary/victim structure. This reading (boundary_maintenance) has the most concentrated victim set (dissenting_members, intermarried_families, out_group_relations) and the clearest active-enforcement gate of the four, which is why it alone claims tangled_rope rather than rope or mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
