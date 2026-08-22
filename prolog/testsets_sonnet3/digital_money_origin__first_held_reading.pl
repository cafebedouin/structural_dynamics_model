% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin — Dated to First Practical Holding by Individuals
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the 'first held' reading of the
 *   digital-money-origin kernel: digital money is dated to the moment
 *   ordinary individuals began practically holding non-physical monetary
 *   instruments (electronic bank balances, then cards, then mobile wallets)
 *   as their actual store of value, rather than to the earlier moment the
 *   concept became technically conceivable (became_thinkable_reading) or the
 *   later moment regulators formally counted it in statistical aggregates
 *   (regulatory_recognition_reading). This reading dates the origin later
 *   than the conceptual reading and earlier than the regulatory reading, and
 *   it structurally foregrounds infrastructure access as the operative
 *   constraint: whoever could open and use an account is inside the origin
 *   story, and whoever could not is outside it — a boundary that tracks
 *   banking and telecom infrastructure rollout rather than technological
 *   invention or regulatory paperwork.
 *
 * KEY AGENTS:
 *   - early_adopter_account_holders: primary beneficiary (moderate/mobile) — gains convenience and narrative centrality
 *   - issuing_banks_and_platforms: agenda_setter (institutional/arbitrage) — designs and gates the holding infrastructure
 *   - payment_infrastructure_operators: beneficiary/agenda_setter (organized/arbitrage) — builds and monetizes the rails
 *   - unbanked_populations: primary payer (powerless/trapped) — excluded from the origin moment by infrastructure access
 *   - rural_low_connectivity_users: payer (powerless/constrained) — intermittent access dates them later
 *   - cash_dependent_informal_workers: payer (powerless/trapped) — bears rising friction as system consolidates around holders
 *   - monetary_historians: analytical observer — chooses among competing dating conventions with real distributive consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.52).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.44).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin — Dated to First Practical Holding by Individuals").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '75db42a6-73b9-4e1b-9587-5eb3e2ba91cc').
narrative_ontology:cs_kernel_codification('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', distributed).
narrative_ontology:cs_authority_grounding('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', distributed).
narrative_ontology:cs_reading_relation('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', foundational, lived_practice_constitutes_monetary_reality).
narrative_ontology:cs_axiom_status(lived_practice_constitutes_monetary_reality, holdable).
narrative_ontology:cs_axiom_grounding('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', lived_practice_constitutes_monetary_reality, empirically_contingent).
narrative_ontology:cs_axiom('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', secondary, access_asymmetry_is_constitutive_of_origin_boundary).
narrative_ontology:cs_axiom_status(access_asymmetry_is_constitutive_of_origin_boundary, holdable).
narrative_ontology:cs_axiom_grounding('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', access_asymmetry_is_constitutive_of_origin_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', practical_holding_as_adoption_benchmark).
narrative_ontology:cs_drift_state('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', post_mobile_money_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75db42a6-73b9-4e1b-9587-5eb3e2ba91cc', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopter_account_holders).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, issuing_banks_and_platforms).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, payment_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, rural_low_connectivity_users).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, cash_dependent_informal_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First individuals to hold electronic balances (bank ledger entries, then card-linked accounts, then mobile wallets) as their practical store of value. They gain convenience, interest-bearing custody, and integration into an expanding payments network at the moment infrastructure becomes available to them.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopter_account_holders, beneficiary,
    moderate, biographical, mobile, national).

% Design and administer the account systems, set the terms of custody, and control who can practically hold digital balances. They benefit from float, fees, and data, and they determine the technical and identity requirements that gate entry into 'holding' digital money at all.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, issuing_banks_and_platforms, agenda_setter,
    institutional, generational, arbitrage, national).

% Card networks, clearing systems, and mobile-money operators that built the rails making practical holding possible. They collect transaction and interchange revenue and jointly set the interoperability standards that determine who counts as an accessible holder.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, payment_infrastructure_operators, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, payment_infrastructure_operators, agenda_setter).

% Excluded from opening accounts by identity documentation requirements, minimum balances, or geographic distance from branches. Under the first-held dating, their absence from the origin moment is structural, not incidental — the constraint's very definition of 'digital money exists' depends on infrastructure access they lack, and they bear downstream costs (higher cash-handling fees, exclusion from credit histories) as the digital system consolidates around holders.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, national).

% Have nominal account access but unreliable network coverage or agent-banking density, making practical holding intermittent. They pay travel and time costs to convert digital balances back to cash, and are systematically dated later into the constraint's own history than urban adopters.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, rural_low_connectivity_users, payer,
    powerless, biographical, constrained, regional).

% Paid and paying in cash by necessity of their economic niche (day labor, informal trade). As formal statistics, credit systems, and merchant infrastructure reorganize around the first-holders, informal workers face rising friction transacting outside the digital system even though they never chose to exit it.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_informal_workers, payer,
    powerless, immediate, trapped, local).

% Study when digital money 'began' and must choose among competing dating conventions — conceptual emergence, first practical holding, or regulatory recognition. Their choice of convention has downstream effects on which populations are treated as historically central versus peripheral to the monetary system's founding narrative.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dating digital money's origin to first practical individual holding coordinates a shared empirical benchmark: it lets economists, historians, and institutions agree on when a monetary instrument moved from possibility to lived practice, anchoring later claims about adoption curves and financial inclusion to an observable event rather than a conceptual or bureaucratic one.
% TRANSFER_FUNCTION: The dating convention itself transfers little directly, but the underlying infrastructure it dates — bank and platform accounts, payment rails — moves custody, float income, and transaction fees from account holders to issuing institutions, and moves narrative centrality (being counted as part of the origin story) from those with access to those without.
% ABSENT_VOICES: Unbanked and rural populations are absent from the moment the constraint itself designates as the origin; their later, partial, or intermittent access to holding means the historical record built on this dating convention structurally underweights their experience and overweights early urban adopters as the story's protagonists.
% DISAPPEARANCE_RATIONALE: If this specific dating convention vanished, monetary historians would still need some benchmark for digital money's emergence; the practical effect would be a shift in which populations and events are treated as foundational. Institutions using 'first held' framing for financial-inclusion metrics would need to re-derive baselines, but the underlying payment infrastructure would be unaffected — so the world of banking rails stays unchanged while the historical narrative and inclusion metrics built atop the dating convention would rearrange.
% FOUNDING_PROBLEM: Economic historians and statisticians needed a defensible, non-arbitrary marker for when 'digital money' stopped being speculative or institutional-internal and became something ordinary people actually used as a store of value, distinguishing genuine adoption from technical possibility or bureaucratic classification.
% FOUNDING_PROBLEM_CORROBORATION: Financial-inclusion researchers and development economists outside the banking industry corroborate that dating by practical holding better tracks real-world access and exclusion than dating by conceptual availability or regulatory statistics — but they also note, from outside the beneficiary set of issuing banks, that this same dating convention has been used by industry reports to claim earlier and broader 'digital adoption' than unbanked populations' actual experience supports.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, contested).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 by 2020) because the coordination function is genuine — a practical, observable benchmark for adoption is analytically useful — but the same dating convention has been leveraged by banks and platforms to claim broader 'financial inclusion' progress than unbanked and rural populations' lived experience supports, and issuing institutions capture float and fee revenue from the very accounts that define the origin moment. Suppression (0.44) reflects the structural barriers (documentation, minimum balances, connectivity) that determine who counts as a holder, not overt coercion. Theater ratio is low-moderate (0.22) and rising slowly, reflecting increasing use of 'first held' framing in industry inclusion reports that outpaces the underlying access reality. All three metrics are authored on the shared 1960–2020 grid.
 *
 * PERSPECTIVAL GAP:
 *   From the issuing-institution seat, the first-held dating is simply the most empirically grounded historical marker available. From the unbanked and rural seats, the same dating convention operates as a mechanism that both excludes them from a foundational narrative and legitimizes claims of inclusion progress that do not reflect their access reality — the engine's per-seat computation should register this asymmetry structurally, not narratively.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and the institutions serving them sit near the beneficiary end: they gained access when infrastructure was available and their holding is what the constraint counts as 'origin.' Unbanked and rural populations sit near the target end precisely because their exclusion from practical holding is what dates them out of the origin narrative and subjects them to downstream friction (documentation burdens, cash-handling costs) as institutions and statistics reorganize around holders. Issuing banks and payment operators are structural agenda-setters with arbitrage-grade exit — they can relocate technical requirements, fee structures, and access gates at will.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than a pure mountain of technological fact or a pure snare) reflects that the coordination function — a genuine, useful empirical marker for adoption research — is real and not merely cover, but it coexists with asymmetric extraction: issuing institutions profit from the infrastructure that defines the marker, and populations excluded from that infrastructure bear real costs from being classified as outside the origin story. Treating this purely as neutral historiography would mislabel real extraction as innocent dating convention; treating it purely as extraction would erase the genuine analytical utility of an observable adoption benchmark.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_dating_convention_choice,
    'Is the ''first practical holding'' moment a structurally privileged historical marker, or is it one contestable convention among three (conceptual availability, practical holding, regulatory recognition) chosen because it best supports certain institutional narratives of inclusion progress?',
    'Compare how each dating convention performs against independent adoption data (e.g., account penetration surveys, informal-economy cash-transaction volumes) to assess whether the first-held convention tracks genuine access shifts or merely tracks institutional record-keeping availability.',
    'If the convention is shown to systematically favor institutional narratives over lived access reality, the coordination function weakens relative to the extraction reading and the classification should move toward snare; if it robustly tracks independent access measures, the coordination function is vindicated and the classification moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_dating_convention_choice, conceptual, 'Whether the first-held dating convention is a neutral empirical marker or an institutionally favorable framing choice among three live alternatives.').

omega_variable(
    kernel_sibling_delta_effect,
    'How much does the choice among the three kernel readings (became_thinkable, first_held, regulatory_recognition) change which populations are historically centered versus marginalized in monetary-inclusion narratives?',
    'Cross-reading comparative analysis: hold the historical record fixed and re-derive ''origin'' dates and populations under each of the three readings, then compare the resulting beneficiary/victim sets.',
    'A large delta across readings would indicate the choice of reading itself is doing significant distributive work (favoring whichever population''s access defines the chosen benchmark); a small delta would indicate the readings converge on similar populations regardless of dating convention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_sibling_delta_effect, conceptual, 'Sensitivity of the beneficiary/victim structure to which of the three kernel readings is adopted as the historical benchmark.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_origin__first_held_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__first_held_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__first_held_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_origin__first_held_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__first_held_reading, base_extractiveness, 1970, 0.33).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__first_held_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.49).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_origin__first_held_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__first_held_reading, suppression_requirement, 1970, 0.33).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__first_held_reading, suppression_requirement, 1985, 0.37).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the digital_money_origin kernel. became_thinkable_reading dates origin earliest (conceptual/technical conceivability, minimal beneficiary/victim structure, closer to a rope or even piton). first_held_reading (this story) dates origin to individual practical holding and introduces infrastructure-access beneficiaries and victims, producing a tangled_rope profile. regulatory_recognition_reading dates origin latest, to formal statistical/regulatory incorporation, with monetary authorities and regulated institutions as the primary parties rather than individual holders. Each story carries its own stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
