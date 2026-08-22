% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Popular Sovereignty / Republican Legitimacy Reading
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the republican reading of the
 *   sovereign_legitimacy kernel: authority is claimed to flow upward from the
 *   people through delegated consent, grounded in social contract theory.
 *   Historically, franchise expansion (property qualifications removed,
 *   universal suffrage, minority voting rights acts) has widened who counts
 *   as 'the people' and reduced the gap between the governed population and
 *   the consenting population — the declining extractiveness and
 *   suppression-requirement series reflect that broadening. But the residual
 *   ε (0.42) and the standing exclusion structure are load-bearing, not
 *   incidental: even mature republics formally exclude non-citizens, minors,
 *   and (in many jurisdictions) certain classes of residents from the consent
 *   mechanism whose legitimacy claim covers them anyway. This is one reading
 *   among three of the sovereign_legitimacy kernel; the monarchical_reading
 *   and constitutional_hybrid_reading are separate constraints with their own
 *   ε and stakeholder sets, linked via network.affects_constraints, not
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - enfranchised_citizens: primary beneficiary and nominal source of authority (organized/constrained)
 *   - elected_officeholders: agenda-setters who administer consent-derived power under term-limited accountability (institutional/constrained)
 *   - majoritarian_coalitions: organized beneficiaries who convert electoral share into durable policy capture (organized/mobile)
 *   - disenfranchised_residents: bear the authority's costs with no formal voice (powerless/trapped)
 *   - structural_minorities: enfranchised but structurally outvoted with no realistic accountability lever (powerless/trapped)
 *   - non_citizen_stakeholders: materially bound, formally absent from the mechanism (powerless/trapped)
 *   - constitutional_courts: analytical/agenda-setting check on majoritarian excess (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.35).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Popular Sovereignty / Republican Legitimacy Reading").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'b283df1d-74f2-4596-a73d-cb52a4a7ce9a').
narrative_ontology:cs_kernel_codification('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', formalized).
narrative_ontology:cs_authority_grounding('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', practice).
narrative_ontology:cs_interpretation_layer_present('b283df1d-74f2-4596-a73d-cb52a4a7ce9a').
narrative_ontology:cs_reading_relation('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', foundational, authority_originates_in_popular_consent).
narrative_ontology:cs_axiom_status(authority_originates_in_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', authority_originates_in_popular_consent, deontological).
narrative_ontology:cs_axiom('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', foundational, legitimacy_requires_periodic_revalidation).
narrative_ontology:cs_axiom_status(legitimacy_requires_periodic_revalidation, holdable).
narrative_ontology:cs_axiom_grounding('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', legitimacy_requires_periodic_revalidation, conventional).
narrative_ontology:cs_axiom('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', secondary, hereditary_transmission_confers_no_authority).
narrative_ontology:cs_axiom_status(hereditary_transmission_confers_no_authority, holdable).
narrative_ontology:cs_axiom_grounding('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', hereditary_transmission_confers_no_authority, deontological).
narrative_ontology:cs_reference_frame('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', popular_sovereignty_social_contract).
narrative_ontology:cs_drift_state('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', contemporary_mass_democracy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b283df1d-74f2-4596-a73d-cb52a4a7ce9a', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizens).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, majoritarian_coalitions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, structural_minorities).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizen_stakeholders).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the vote and the standing to petition, organize, and remove officeholders through electoral cycles. Their consent is the formal source cited for the state's authority; in practice their influence is mediated through aggregation mechanisms (districting, party structures, campaign finance) that some exercise more effectively than others. Exit means emigration or civil disobedience, both costly.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizens, agenda_setter).

% Derive authority from periodic electoral validation and administer the machinery of consent — drafting law, running agencies, interpreting mandates. Face removal at defined intervals, which disciplines but does not eliminate self-interested drift between elections. Benefit from incumbency advantages the consent framework does not fully price.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officeholders, agenda_setter,
    institutional, biographical, constrained, national).

% Assemble sufficient electoral share to control outcomes and translate that share into durable policy and institutional capture between elections. The consent framework legitimates whatever this coalition enacts, without requiring supermajority buy-in from those outside it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, majoritarian_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Live under laws and enforcement they had no formal vote in shaping — residents below voting age, those with felony disenfranchisement, undocumented residents, colonial or territorial subjects of the polity. The consent story does not include them as a source of authority even though the authority binds them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Are enfranchised but structurally outvoted on matters central to their interests every cycle, with no realistic path to majority coalition. The removal mechanism that legitimates the system for the majority offers this group no real accountability lever — the majority can be voted out, but policies targeting the minority persist across administrations.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, structural_minorities, payer,
    powerless, generational, trapped, national).

% Are materially affected by the polity's laws — trade partners, cross-border residents, future generations bound by long-lived legislation — but have no franchise and no standing in the consent mechanism at all. Their interests enter only if some enfranchised actor chooses to represent them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_citizen_stakeholders, excluded,
    powerless, immediate, trapped, national).

% Adjudicate whether majoritarian action stays within the constitutional bounds meant to check majority tyranny, interpreting the consent framework's own limiting principles. Can invalidate what electoral majorities enact, creating friction with the pure upward-flow account of authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed individual preferences into binding collective decisions through a periodic, rule-governed procedure (elections), allowing large, pluralistic populations to change rulers and policy without violence and to withdraw consent from officeholders who lose the mandate.
% TRANSFER_FUNCTION: Moves decision-making authority from individuals to representatives for a fixed term, and moves policy costs/benefits according to whichever coalition assembles electoral majority — the transfer is time-boxed and formally reversible, but reversal requires organizing a new majority, which is easier for some groups than others.
% ABSENT_VOICES: Disenfranchised residents, structural minorities without realistic coalition paths, and non-citizen stakeholders bound by the polity's decisions have no seat in the consent mechanism the legitimacy claim is built on; they would object that 'the people' as counted is narrower than 'the people' as governed.
% DISAPPEARANCE_RATIONALE: If electoral consent mechanisms disappeared overnight, officeholders would have no formal removal mechanism or renewal procedure; the entire apparatus of campaigns, legislatures, and terms of office would collapse or be replaced by an alternative legitimacy claim (inherited, military, technocratic). Coalitions currently governing through majority-building would need an entirely different route to power.
% FOUNDING_PROBLEM: Hereditary and divine-right rule left populations with no formal mechanism to remove rulers who governed badly or against their interests, and no principled basis for authority other than birth or conquest; popular sovereignty was built to ground authority in the consent of the governed and provide a peaceful, periodic mechanism for correction.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and comparative-democracy scholars outside the electoral coalitions attest that the removal mechanism remains functionally live in most republics (turnover happens, elections are contested). But those same outside observers, along with disenfranchisement-reform advocates, corroborate that the franchise itself has never matched the governed population, and that structural minorities and non-citizen stakeholders experience the 'solved' problem as unsolved for them specifically — the founding problem is live for the excluded even where it reads as resolved for the enfranchised majority.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine — periodic elections do transfer removal power to a broad citizenry and this is not mere theater. It is not low because the franchise boundary is itself extractive: it draws a line between consenting and governed populations that the legitimacy claim papers over. Suppression (0.35) captures the residual coercive floor — laws bind the disenfranchised without their consent, and structural minorities cannot exit the majoritarian outcome even though the formal mechanism is non-coercive for the majority. Theater ratio (0.28) is moderate: the electoral apparatus does real accountability work, but a growing share of activity (campaign spectacle, symbolic representation gestures toward excluded groups) is performative relative to the underlying inclusion problem. The suppression_requirement trend falls over the interval as franchise expanded historically; the falling trend is honest to that history, not tuned to the endpoint.
 *
 * PERSPECTIVAL GAP:
 *   From the enfranchised-citizen and majoritarian-coalition seats, this looks like Rope: a working coordination mechanism they can exit any incumbent through. From the disenfranchised-resident and structural-minority seats, the same formal structure is closer to Tangled Rope or worse — coordination for some, unremovable extraction for others, run through the identical electoral machinery. The engine should compute this divergence directly from the beneficiary/victim/exit declarations; the claimed_type (tangled_rope) reflects the structural coexistence of both functions rather than adjudicating between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizens and majoritarian coalitions get low d (beneficiary end) because they hold the vote, aggregate into governing coalitions, and can exit incumbents through the mechanism itself. Elected officeholders sit closer to symmetric — they administer power but are also disciplined by removal. Disenfranchised residents, structural minorities, and non-citizen stakeholders get high d (target end): they are bound by the authority's output, have trapped exit options, and the consent mechanism that legitimates the system for others provides them no lever at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary hereditary rule with no removal mechanism — is genuinely dead for the enfranchised population; elections work, turnover happens, this is not zombie legitimacy for the majority. But mandatrophy analysis at the margin shows the mandate persisting past its justification for the excluded classes: the arrangement continues to claim universal legitimacy ('government by consent of the governed') while the consent pool has never matched the governed pool. Classifying this as tangled_rope rather than snare or rope prevents both errors: it is not pure extraction (the coordination function is real and historically expanding), and it is not pure coordination (the exclusion is structural, not incidental, and requires active enforcement — franchise law, citizenship law, districting law — to maintain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_boundary_naturalness,
    'Is the boundary of ''the people'' whose consent grounds authority a natural, principled boundary (citizenship, capacity for reasoned choice) or a contingent, historically contested line that could be redrawn to reduce the excluded population?',
    'Comparative historical analysis of franchise expansion episodes (property qualifications, race, sex, age, felony status, residency) and whether each expansion reduced measured extraction/suppression without destabilizing the coordination function.',
    'If the boundary is contingent and manipulable, current exclusions (non-citizens, disenfranchised felons, minors materially affected by long-lived policy) are extraction dressed as principled limitation, raising the effective ε for those groups. If some boundary is principled and irreducible, part of the residual extraction is an intrinsic feature of any workable consent mechanism, not a fixable defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_naturalness, conceptual, 'Whether the franchise boundary is a natural limit on consent or a constructed exclusion.').

omega_variable(
    majoritarian_tyranny_check_adequacy,
    'Do constitutional courts and minority-protection mechanisms adequately correct for the republican reading''s structural vulnerability to majoritarian tyranny, or do they merely provide theatrical cover while majoritarian coalitions capture policy durably?',
    'Track rates at which constitutional review actually overturns majoritarian legislation targeting structural minorities, versus rates at which such legislation persists uncorrected across multiple electoral cycles.',
    'If courts substantively correct majoritarian overreach, the tangled_rope classification''s extractive component is smaller and more self-correcting than authored. If courts rarely intervene or intervene only symbolically, theater_ratio is understated and structural minorities'' effective χ is higher than the base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_check_adequacy, empirical, 'Whether constitutional checks meaningfully limit majoritarian extraction from structural minorities.').

omega_variable(
    sibling_reading_kernel_disagreement_locus,
    'Where exactly does this reading''s disagreement with the monarchical_reading and constitutional_hybrid_reading live — is it a disagreement about the SOURCE of authority (people vs. sovereign vs. dual), or about the VALIDATION mechanism (elections vs. tradition vs. mixed), or both?',
    'Compare the three constraint files'' beneficiary/victim structures and axioms directly: if the disagreement were purely about validation mechanism, the beneficiary sets would substantially overlap across readings; if purely about source, the axioms would show direct logical contradiction.',
    'Determines whether reading_relations to siblings should lean toward forecloses (source-level contradiction, e.g. ''authority flows from the people'' vs. ''authority flows from bloodline'' cannot both be the grounding claim) or coexists_with/influences (different validation mechanisms operating within compatible source theories, as in the hybrid reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_disagreement_locus, conceptual, 'Where the committer disagreement across kernel readings is structurally located.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__republican_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.24).
narrative_ontology:measurement(sove_tr_t150, sovereign_legitimacy__republican_reading, theater_ratio, 150, 0.26).
narrative_ontology:measurement(sove_tr_t200, sovereign_legitimacy__republican_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement(sove_tr_t250, sovereign_legitimacy__republican_reading, theater_ratio, 250, 0.28).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__republican_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement(sove_be_t150, sovereign_legitimacy__republican_reading, base_extractiveness, 150, 0.44).
narrative_ontology:measurement(sove_be_t200, sovereign_legitimacy__republican_reading, base_extractiveness, 200, 0.43).
narrative_ontology:measurement(sove_be_t250, sovereign_legitimacy__republican_reading, base_extractiveness, 250, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__republican_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement(sove_su_t150, sovereign_legitimacy__republican_reading, suppression_requirement, 150, 0.38).
narrative_ontology:measurement(sove_su_t200, sovereign_legitimacy__republican_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement(sove_su_t250, sovereign_legitimacy__republican_reading, suppression_requirement, 250, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sovereign_legitimacy kernel. sovereign_legitimacy__monarchical_reading grounds authority downward through inherited right and divine sanction (beneficiary: hereditary elite; victim: the governed with no removal mechanism at all — likely higher ε and near-zero accountability). sovereign_legitimacy__constitutional_hybrid_reading splits authority into inherited ceremonial and delegated political components, mediated by constitutional law (a distinct beneficiary/victim structure again). Each reading has its own stable ε assessed by that reading's own lights, per the ε-invariance and kernel-reading-referent rules; none of the three averages over or references the others' ε values directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
