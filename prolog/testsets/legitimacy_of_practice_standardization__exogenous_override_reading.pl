% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Mandated Practice Standardization via Exogenous Authority Override
 *   domain: political/institutional
 *
 * SUMMARY:
 *   A state authority decrees the standardization of practice (calendar
 *   system, dress code, measurement units, legal procedure) in the name of
 *   modernization, fiscal efficiency, and international alignment. The decree
 *   is presented as enabling coordination across dispersed populations; rural
 *   populations experience it as the exogenous replacement of ancestral
 *   practice with externally imposed standards. The key structural finding:
 *   rural populations achieve a stable double-practice equilibrium (public
 *   compliance, private continuity) rather than genuine adoption. This is NOT
 *   a transitional phase — the underground maintenance persists for decades,
 *   coexisting with surface conformity. The constraint operates as enforced
 *   extraction justified by coordination legitimacy: real coordination gain
 *   at the center, real identity/role disruption at the periphery,
 *   enforcement overhead steadily rising as initial legitimacy gains plateau
 *   and the arrangement persists primarily through suppression. This story
 *   instantiates one reading of the contested kernel
 *   'legitimacy_of_practice_standardization': the exogenous override reading
 *   asserts that state authority has the right to decree practice
 *   standardization for collective benefit, regardless of local practice
 *   traditions.
 *
 * KEY AGENTS:
 *   - State administrative apparatus: institutional power; sets the decree and enforcement machinery; derives legitimacy from modernization success
 *   - Rural populations: powerless; trapped in local geography; bear the cost of identity disruption and ritual discontinuity; maintain underground practice as stable equilibrium
 *   - Modernizing intelligentsia: powerful; beneficiaries; occupy state roles; face minimal personal cost; mobile exit options
 *   - International alignment beneficiaries: institutional; benefit from externally imposed synchronization without bearing enforcement cost
 *   - Tradition practitioners: moderate power; identity-locked to ancestral practice; maintain underground teaching; delegitimized by decree
 *   - Excluded peasant coalition: powerless; geographically dispersed; structurally unable to mount coordinated resistance; would demand domain partitioning if organized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.71).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Mandated Practice Standardization via Exogenous Authority Override").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political/institutional").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'fb446d06-4b02-4fcc-8187-de646be0b7b2').
narrative_ontology:cs_kernel_codification('fb446d06-4b02-4fcc-8187-de646be0b7b2', formalized).
narrative_ontology:cs_authority_grounding('fb446d06-4b02-4fcc-8187-de646be0b7b2', extraction).
narrative_ontology:cs_interpretation_layer_present('fb446d06-4b02-4fcc-8187-de646be0b7b2').
narrative_ontology:cs_reading_relation('fb446d06-4b02-4fcc-8187-de646be0b7b2', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb446d06-4b02-4fcc-8187-de646be0b7b2', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_axiom('fb446d06-4b02-4fcc-8187-de646be0b7b2', foundational, state_authority_supremacy_in_modernization).
narrative_ontology:cs_axiom_status(state_authority_supremacy_in_modernization, holdable).
narrative_ontology:cs_axiom_grounding('fb446d06-4b02-4fcc-8187-de646be0b7b2', state_authority_supremacy_in_modernization, empirically_contingent).
narrative_ontology:cs_axiom('fb446d06-4b02-4fcc-8187-de646be0b7b2', foundational, collective_benefit_justifies_exogenous_override).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_exogenous_override, holdable).
narrative_ontology:cs_axiom_grounding('fb446d06-4b02-4fcc-8187-de646be0b7b2', collective_benefit_justifies_exogenous_override, instrumental).
narrative_ontology:cs_reference_frame('fb446d06-4b02-4fcc-8187-de646be0b7b2', state_administrative_supremacy_in_modernization).
narrative_ontology:cs_drift_state('fb446d06-4b02-4fcc-8187-de646be0b7b2', stable_double_practice_equilibrium_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb446d06-4b02-4fcc-8187-de646be0b7b2', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_beneficiaries).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, tradition_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_intelligentsia).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, tradition_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues decrees standardizing practices (calendars, dress codes, measurement systems) in the name of modernization, fiscal efficiency, and international alignment. Justifies the mandates as necessary for state coherence and competitive positioning. Deploys enforcement mechanisms (inspections, penalties, curriculum revision) to ensure compliance. Collects no direct rent but derives institutional legitimacy from successfully modernizing the polity.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of abandoning ancestral practices (lunar calendar, traditional dress, local measurement systems) that are embedded in ritual, agricultural cycle coordination, and identity. Face inspections, fines, or social sanction for continued private practice. Maintain underground compliance — surface adoption of state-mandated practice while continuing ancestral practice in private/ritual domains — as a stable equilibrium, not a transitional phase. Exit is impossible: geographic mobility is limited, and alternatives for ritual coordination do not exist.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations, payer,
    powerless, generational, trapped, local).

% Benefit from standardization as a signal of national sophistication and integration into international modernity. Often occupy state or educational roles where enforcing the new standards enhances their status and career prospects. Face minimal personal cost from abandoning traditional practice because they have already adopted modern alternatives or never depended on them. Their exit options include international migration or withdrawal from state employment.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernizing_intelligentsia, beneficiary,
    powerful, biographical, mobile, national).

% International trading partners, colonial authorities, or development institutions that condition market access or aid on standardized practices (metric system, Gregorian calendar, Western-derived legal procedure). Benefit from the state's enforcement of alignment, which reduces transaction costs and creates regulatory synchronization. Never directly pay the enforcement cost — that falls on rural populations within the nation-state.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_beneficiaries, beneficiary,
    institutional, generational, analytical, global).

% Community leaders, priests, and elders whose authority depends on mastery and transmission of ancestral practices. Mandated change directly delegitimizes their expertise and undermines their social role. They maintain underground teaching of traditional practice, creating a dual-knowledge system: public conformity, private continuity. Identity fusion with the tradition (profession, status, self-concept) makes exit psychologically impossible despite structural barriers.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, tradition_practitioners, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, tradition_practitioners, beneficiary).

% Would articulate a demand for decentralized, place-based practice standards that honor local ecological knowledge and ritual coordination if organized into a voice; instead remain geographically dispersed and administratively fractured, unable to mount coordinated resistance or petition for exemption. Their absence from policy-making forums is structural, not incidental.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, excluded_peasant_coalition, excluded,
    powerless, biographical, trapped, local).

% Researchers and institutions studying whether mandated practice change produces genuine cultural integration or stable double-practice equilibrium, and what the relationship is between exogenous decree and endogenous adoption. Witness the constraint's actual operation across multiple cases without power to alter it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, comparative_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes single administrative practice space (calendar, measurement, dress code) across dispersed populations: enables unified tax collection, synchronized communication with central authority, and alignment with international trading and diplomatic standards.
% TRANSFER_FUNCTION: Transfers authority to define legitimate practice from local/traditional authorities to state administrative authority; extracts identity disruption, ritual discontinuity, and social role delegitimization from rural and tradition-keeper populations; moves legitimacy gains and administrative/international benefits to state administrators and international alignment beneficiaries.
% ABSENT_VOICES: Peasant associations, tradition-keeper guilds, and underground practice communities would demand domain-partitioned practice legitimacy (state standards in administrative domains, traditional authority in private/ritual domains) if organized into collective voice; their dispersal and administrative exclusion structure their silence. Rural literacy barriers also prevent easy entry into policy debates conducted in state languages and formats.
% DISAPPEARANCE_RATIONALE: If the state decree and enforcement apparatus vanished overnight, rural and tradition-keeper populations would openly resume ancestral practices within weeks. The public-private double-practice partition would dissolve and practices would coexist in all domains, not hidden ones. International alignment would require renewed bilateral negotiation rather than enforced internal standardization. State administrative capacity would require retooling to handle heterogeneous local practices. The social role and authority of tradition practitioners would be restored, though the modern techniques adopted in administrative domains would persist as acquired tools.
% FOUNDING_PROBLEM: Dispersed, heterogeneous local practices (lunar vs. Gregorian calendars, local vs. metric measurement systems, regional dress codes) prevent legible tax administration, synchronized state communication, and integration into international markets and diplomatic coordination.
% FOUNDING_PROBLEM_CORROBORATION: State authorities testifying that administrative legibility requires standardized practices and that standardization has been achieved by mid-interval. International trading partners and colonial/development institutions testifying that practice alignment has enabled market integration and diplomatic efficiency. Rural population testimony and anthropological research testifying that the foundational coordination problem (administrative legibility) is solved — the state CAN levy taxes and communicate without requiring permanent enforcement of cultural conformity. What persists is the state's enforcement of cultural standardization for its own institutional legitimacy, not for the founding coordination problem. This attests to mandatrophy: the founding problem is solved but constraint persists.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory shows a classic pattern of exogenous decree followed by ceiling-out. Initial decree extractiveness is moderate (0.38) because the coordination function is real — synchronization genuinely solves legibility and international alignment problems. But as the interval progresses, extractiveness rises toward the final state (0.68) as continued enforcement shifts from legitimacy-driven to suppression-driven. Theater ratio shows the same pattern: rises from 0.12 (genuine coordination activity) to 0.42 (enforcement and theater increasingly dominate actual function). Suppression requirement rises and plateaus (0.48 to 0.71), indicating that initial legitimacy carries enforcement but cannot sustain it without growing coercive overhead. The key observation: extractiveness plateaus at 0.68, not because the constraint is resolved, but because the stable double-practice equilibrium is reached — rural populations no longer require escalating suppression; they have internalized the double-practice norm. The theater ratio continues to rise because surface-level state ritual about modernization becomes more performative as underground practice stabilizes. This reading describes the exogenous-override constraint under the assumption that state authority's decree legitimacy is accepted as binding; it does NOT adjudicate whether that legitimacy claim is justified.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's fundamental asymmetry is that the state authority and its beneficiaries define the legitimacy narrative (coordination, modernization, necessity), while the costs fall almost entirely on the powerless rural seat and the identity-locked tradition-keeper seat. The double-practice equilibrium is stable precisely because enforcement overhead and underground practice coexist indefinitely — neither side achieves final victory. The state's enforcement capacity is sufficient to prevent overt rebellion (resistance is suppressed or channeled into underground practice) but insufficient to eliminate the underground practice without escalating surveillance to unbearable levels. Rural populations' resistance capacity is sufficient to maintain ancestral knowledge and practice in private domains but insufficient to openly challenge state decree. This structural stalemate persists because the suppression requirement (0.71 at endpoint) is stable — the state can afford it, rural populations can bear it through the double-practice compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: beneficiary (d ≈ 0.05). The decree increases state administrative capacity and international legitimacy; the state incurs enforcement overhead but enjoys compensating institutional prestige. Its exit options are analytical — it could reverse the decree but chooses not to as long as international alignment benefits persist. Rural populations: full target (d ≈ 0.95). They bear the direct cost of identity disruption, ritual discontinuity, and surveillance; they have no exit (trapped by geography and economic dependence); their suppression is both structural (legal penalties for non-conformity) and internalized (belief that the state's legitimacy to decree is binding, even when resisted). Tradition practitioners: near-full target (d ≈ 0.88) because their authority is directly delegitimized, but slightly elevated from rural d because their moderate institutional power allows some underground work without surveillance. Modernizing intelligentsia: near-beneficiary (d ≈ 0.15) because they benefit from modernization signaling and career mobility but bear diffuse costs (participation in enforcement, social tension with rural kin) and have mobile exit options (can leave the state apparatus). International alignment beneficiaries: beneficiary (d ≈ 0.08) because they gain standardization without bearing enforcement cost — a perfect structural subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy (decay of mandate) is the key analytic signal. The founding problem (dispersed practice prevents legible administration and international alignment) is partially solved and then SOLVED — by the mid-interval (time_point 21), administrative legibility and international alignment are achieved. But the constraint persists because enforcement machinery has become self-sustaining: inspectors have careers built on enforcement, state legitimacy is now tied to the achievement of conformity, and the cost of reversal (admitting the decree was wrong, resigning careers, renegotiating international agreements) exceeds the benefit of lifting it. The theater ratio rise (0.12 to 0.42) is the smoking gun: enforcement activity increasingly performs conformity rather than producing it. Underground practice is stable at both time_point 35 and 50; the state knows this and enforces anyway. The mandate has decayed from 'we must achieve practice standardization because it solves legibility' to 'we must enforce practice standardization because we have always enforced it and reversal would delegitimize the state.' This is the classic double-life of an extractive constraint wearing a coordination mask: real coordination function at founding, persistence divorced from that function by mid-interval, theater dominating by endpoint. The constraint should be reclassified or at minimum audited for mandatrophy: founding_problem_status = dead (administrative legibility is achieved and stable without further decree), but constraint persists at high suppression cost. This is the pattern tangled_rope transitions into snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforced_vs_internalized_suppression,
    'Is the suppression that maintains double-practice equilibrium primarily structural (legal penalties, surveillance capacity, economic dependence on state systems) or internalized (rural populations believe the state''s decree legitimacy despite resistance)?',
    'Post-enforcement relaxation analysis: if suppression is removed entirely (amnesty for underground practice, no penalties), how quickly does underground practice emerge into public view? If it emerges within weeks, suppression was primarily structural; if it remains hidden for months or years even without enforcement, suppression is substantially internalized (identity-fusion with compliance, belief in state authority).',
    'If primarily structural, the constraint''s effective suppression is as authored (0.71); if substantially internalized, the true suppression is higher — the target carries the suppression forward even without external enforcement. This affects exit-option reassessment: identity-locked rural populations may have less trapped exit than measured if suppression is purely structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforced_vs_internalized_suppression, empirical, 'Whether double-practice equilibrium reflects structural coercion or internalized legitimacy of state authority.').

omega_variable(
    founding_problem_resolution_status,
    'Is the founding problem (dispersed practice prevents legible administration) still live at the interval endpoint, or was it solved by time_point 21 and the constraint persists for institutional inertia reasons?',
    'Audit state administrative performance: can the state collect taxes, synchronize communication, and maintain international alignment with the current dual-practice regime (surface conformity + underground practice)? If yes, the founding problem is DEAD and the constraint is operating under mandatrophy. If administrative coherence would collapse without ongoing enforcement, the founding problem is LIVE.',
    'If founding problem is dead, the constraint transitions from tangled_rope (coordination + extraction) to piton (extraction + theater, no coordination function). This would trigger a mandatrophy_resolved gate and recommend reclassification or remediation. If founding problem is live, the constraint remains tangled_rope throughout the interval.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_resolution_status, empirical, 'Whether the founding coordination problem remains unresolved or has been solved but enforcement persists.').

omega_variable(
    domain_partition_viability,
    'Would a domain-partitioned practice regime (state standards in administrative/public domains, traditional practice in private/ritual domains) solve both the administrative coherence problem AND eliminate the identity disruption extraction?',
    'Comparative case analysis of jurisdictions that permit dual-practice in segregated domains (e.g., Gregorian calendar for administration, lunar calendar for religious/agricultural ritual). Do these achieve administrative legibility and international alignment while eliminating the suppression requirement?',
    'If domain partition is viable, the exogenous-override reading''s core claim (state decree for collective benefit is necessary and legitimate) is undermined. The constraint would be reclassifiable as coercive extraction masquerading as coordination. This directly contradicts the axiom state_authority_supremacy_in_modernization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_viability, empirical, 'Whether the coordination benefits of standardization require exogenous override or can be achieved through domain partitioning.').

omega_variable(
    endogenous_adoption_counterfactual,
    'Had the state permitted voluntary adoption of modernized practices rather than mandating them, would the same degree of practice standardization have emerged over equivalent time, or does exogenous decree accelerate adoption substantially?',
    'Natural experiment: jurisdictions that used incentives (prestige, career benefits, resource access) for practice adoption vs. jurisdictions that used mandates. Measure standardization rates and suppression requirements at equivalent intervals.',
    'If voluntary adoption achieves comparable standardization, the decree is extractive overlay without coordination benefit — mandatrophy from the founding. If voluntary adoption produces substantially slower or incomplete standardization, the decree provides genuine coordination benefit, though still extractive to identity-locked populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_adoption_counterfactual, empirical, 'Whether the coordination gains of practice standardization require exogenous mandate or would emerge endogenously at lower suppression cost.').

omega_variable(
    reading_contest_foreclosure,
    'Does THIS reading (exogenous-override) logically foreclose the endogenous-displacement reading, or can both readings coexist as different parties'' legitimate framings of the same constraint?',
    'Logical analysis: The exogenous-override reading asserts state authority CAN decree practice standardization; the endogenous-displacement reading asserts that legitimacy REQUIRES voluntary adoption. Can a single framework hold both claims (e.g., ''state has the power to decree, but legitimacy comes from subsequent voluntary adoption'')? If yes, the readings coexist; if no, one forecloses the other.',
    'If exogenous-override forecloses endogenous-displacement, only one reading can be true of the kernel, and the sibling readings form a singular contest. If they coexist, the kernel admits multiple valid framings held by different parties. This affects which reading_relation type is correct (forecloses vs. coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the exogenous-override and endogenous-displacement readings are logically incompatible or can coexist as different parties'' legitimate interpretations of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t7, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 7, 0.16).
narrative_ontology:measurement_basis(legi_tr_t7, observed).
narrative_ontology:measurement(legi_tr_t14, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 14, 0.22).
narrative_ontology:measurement_basis(legi_tr_t14, observed).
narrative_ontology:measurement(legi_tr_t21, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 21, 0.28).
narrative_ontology:measurement_basis(legi_tr_t21, observed).
narrative_ontology:measurement(legi_tr_t28, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement_basis(legi_tr_t28, observed).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 35, 0.39).
narrative_ontology:measurement_basis(legi_tr_t35, observed).
narrative_ontology:measurement(legi_tr_t42, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement_basis(legi_tr_t42, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t7, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement_basis(legi_be_t7, observed).
narrative_ontology:measurement(legi_be_t14, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement_basis(legi_be_t14, observed).
narrative_ontology:measurement(legi_be_t21, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement_basis(legi_be_t21, observed).
narrative_ontology:measurement(legi_be_t28, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 28, 0.64).
narrative_ontology:measurement_basis(legi_be_t28, observed).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(legi_be_t35, observed).
narrative_ontology:measurement(legi_be_t42, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement_basis(legi_be_t42, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t7, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 7, 0.54).
narrative_ontology:measurement_basis(legi_su_t7, observed).
narrative_ontology:measurement(legi_su_t14, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 14, 0.59).
narrative_ontology:measurement_basis(legi_su_t14, observed).
narrative_ontology:measurement(legi_su_t21, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 21, 0.64).
narrative_ontology:measurement_basis(legi_su_t21, observed).
narrative_ontology:measurement(legi_su_t28, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 28, 0.68).
narrative_ontology:measurement_basis(legi_su_t28, observed).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement_basis(legi_su_t35, observed).
narrative_ontology:measurement(legi_su_t42, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement_basis(legi_su_t42, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(legi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'legitimacy_of_practice_standardization.' The kernel is the state authority's claim to the right to standardize practices for collective benefit. Three readings decompose this kernel into structurally distinct constraints: (1) exogenous_override_reading (this file) — state authority's decree legitimacy is binding regardless of voluntary adoption; (2) dual_practice_equilibrium_reading — legitimacy is domain-partitioned; state authority governs public/administrative domains, traditional authority governs private/ritual domains; (3) endogenous_displacement_reading — legitimacy requires voluntary adoption driven by perceived utility; exogenous decree cannot ground legitimacy. These readings have different beneficiary structures, different ε values, and different enforcement implications. They are not alternative measurements of one constraint; they are three constraints instantiated from one contested kernel, held live by different parties in the same historical moment. The exogenous-override reading describes the state authority's framing and the structural consequences of enforcing that framing; it does NOT adjudicate whether the framing is justified. The family decomposition is necessary because a single ε-value cannot capture the kernel's fundamental contest: the state reads standardization decree as coordination (~0.3-0.4 ε on the coordination margin) while rural populations read it as pure extraction (~0.8-0.9 ε on the suppression margin). These are not observer-relative observations of the same constraint; they are two different constraints both riding on the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
