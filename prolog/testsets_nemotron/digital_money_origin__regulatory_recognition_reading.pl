% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money Origin — Regulatory Recognition Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the regulatory_recognition_reading of
 *   the digital_money_origin kernel. It holds that digital money 'emerged'
 *   only when monetary authorities (central banks, treasuries,
 *   standard-setters like the IMF/BIS) formally incorporated digital
 *   instruments into monetary aggregates (M1, M2, broad money) and brought
 *   them inside the regulatory perimeter (e-money directives, payment
 *   services directives, stablecoin frameworks). The reading positions the
 *   origin at the latest plausible date — the moment of state sanction — and
 *   treats the constraint set as dominated by legal/regulatory barriers.
 *   Beneficiaries are the incumbents who gain perimeter protection; victims
 *   are innovators who are excluded until they comply.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.72).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin — Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '869ee1f5-6699-4d88-a8f9-a3ee4bbcb227').
narrative_ontology:cs_kernel_codification('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', formalized).
narrative_ontology:cs_authority_grounding('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', extraction).
narrative_ontology:cs_interpretation_layer_present('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227').
narrative_ontology:cs_reading_relation('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_axiom('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', foundational, monetary_authority_defines_money).
narrative_ontology:cs_axiom_status(monetary_authority_defines_money, holdable).
narrative_ontology:cs_axiom_grounding('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', monetary_authority_defines_money, conventional).
narrative_ontology:cs_axiom('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', secondary, regulatory_perimeter_protects_stability).
narrative_ontology:cs_axiom_status(regulatory_perimeter_protects_stability, holdable).
narrative_ontology:cs_axiom_grounding('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', regulatory_perimeter_protects_stability, instrumental).
narrative_ontology:cs_reference_frame('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', pre_regulatory_digital_instruments).
narrative_ontology:cs_drift_state('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', post_stablecoin_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('869ee1f5-6699-4d88-a8f9-a3ee4bbcb227', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, regulated_payment_processors).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, crypto_asset_issuers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, decentralized_finance_participants).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, financial_stability_mandate).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, regulatory_perimeter_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the statistical aggregates and regulatory frameworks that determine what counts as money. They issue licenses, set reporting standards, and enforce the perimeter. Their recognition legitimizes digital instruments as money and their exclusion delegitimizes alternatives. They collect seigniorage and regulatory authority from maintaining the monetary order.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Banks and established payment networks that hold regulatory licenses and operate within the recognized perimeter. They benefit from the barrier to entry that regulatory recognition creates — unregulated competitors cannot offer 'money' services without authorization. They participate in writing the standards through industry lobbying and formal consultation.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, agenda_setter).

% Fintech firms that have obtained licenses (e-money, payment institution, banking) and operate inside the regulatory perimeter. They benefit from the credibility and network access that recognition confers, but bear compliance costs. Their position is precarious: they are close enough to the perimeter to be regulated, but depend on the perimeter holding to justify their license value.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, regulated_payment_processors, beneficiary,
    organized, biographical, mobile, global).

% Builders of novel digital monetary instruments (stablecoins, tokenized deposits, CBDC alternatives) that operate outside or at the edge of current regulatory recognition. They bear the cost of legal uncertainty, enforcement risk, and exclusion from payment rails. Their innovations are treated as 'not money' until authorities say otherwise — a structural delay that incumbents do not face.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Issuers of cryptocurrencies and algorithmic stablecoins that explicitly reject the regulatory perimeter. They are structurally excluded from the 'money' category by the recognition constraint — their instruments are classified as assets, commodities, or securities, never money. Exit means abandoning the anti-establishment identity that constitutes their project; many are identity-locked into non-recognition.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, crypto_asset_issuers, payer,
    moderate, biographical, identity_locked, global).

% Users and builders of DeFi protocols who interact with unregulated digital money daily. They bear the costs of regulatory ambiguity: frozen funds, unclear tax treatment, no deposit insurance, no recourse. They are excluded from the conversation about monetary definition — the perimeter is drawn by authorities and incumbents, not by users of the instruments.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, decentralized_finance_participants, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, decentralized_finance_participants, excluded).

% Scholars who study the emergence of digital money across readings. They see the contest over origin dates as a contest over what monetary authority means. Their analysis does not collect rents or bear extraction; it maps the structural field.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative definition of what counts as money for statistical, legal, and settlement purposes — so that contracts, accounts, and policy can reference a stable monetary unit without bilateral negotiation over each instrument's status.
% TRANSFER_FUNCTION: Moves legitimacy, legal tender access, and settlement finality from unregulated innovators to the recognized perimeter. The transfer is not primarily monetary — it is the transfer of the 'money' label itself, which unlocks central bank facilities, deposit insurance, and payment system membership.
% ABSENT_VOICES: End-users in emerging economies who use unregulated digital dollars (stablecoins) as daily money because their local currency fails them. They are not in the room when authorities define money; they simply use what works. Their practical monetary behavior precedes and contradicts the regulatory timeline.
% DISAPPEARANCE_RATIONALE: If regulatory recognition ceased to be the gate for 'money' status, multiple parallel monetary systems would operate with competing definitions. Settlement would fragment, statistical aggregates would lose coherence, and the monetary authority's macroeconomic tools would degrade. The world would rearrange around plural monetary definitions.
% FOUNDING_PROBLEM: The proliferation of private digital instruments (early e-money, stored value cards, proto-stablecoins) in the 1990s–2000s created confusion about what constituted the money supply, undermined monetary statistics, and threatened the transmission of monetary policy.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archives (ECB 1998 report on electronic money, Fed 2000 payments system studies) document the statistical and policy motive. Critics (Selgin, White, and the free banking literature) attest the problem was constructed to justify perimeter expansion — the 'confusion' was manageable without regulatory capture. The corroboration is split along the kernel's reading lines.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial because the regulatory perimeter transfers economic rents (seigniorage, license value, settlement access) from excluded innovators to recognized entities. Suppression (0.72) is high because the constraint actively deploys enforcement (licensing, sanctions, de-banking) to maintain the perimeter. Theater (0.38) is moderate — the consumer protection and stability rationales are real but increasingly serve as cover for rent protection. Accessibility collapse (0.61) reflects that once an instrument is deemed 'not money,' it cannot easily re-enter the monetary circuit without regulatory approval. Resistance (0.54) is significant — crypto, DeFi, and stablecoin ecosystems actively contest the perimeter.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the constraint is coordination: a single monetary definition enables policy transmission. From the innovator seats, the same structure is extraction: the definition is a moving goalpost that protects incumbents. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges both functions are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and incumbents sit at the beneficiary end (d ≈ 0.1–0.2): they set the rules and collect the rents. Regulated processors are near-symmetric (d ≈ 0.45): they pay compliance costs but gain perimeter access. Unregulated innovators and crypto issuers are at the target end (d ≈ 0.75–0.9): they bear exclusion costs, enforcement risk, and identity-lock (for crypto) or trapped status (for DeFi users). DeFi participants are the most extracted-from — powerless, immediate horizon, trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statistical coherence, policy transmission) was live in the 1990s but is contested now. The perimeter has expanded beyond the original motive — stablecoins and tokenized deposits are statistically measurable and policy-transmissible without the full banking license. The constraint persists because the perimeter itself generates rents for those who administer it. Mandatrophy is unresolved: the coordination function has attenuated while the extraction function has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the regulatory_recognition_reading represent a structurally distinct constraint from the became_thinkable_reading and first_held_reading, or are they measurement perspectives on one constraint?',
    'Test ε-invariance: if the three readings author materially different extractiveness/suppression profiles, different beneficiary/victim sets, and different claimed types, they are distinct constraints per DP-001. The engine will classify each independently.',
    'If distinct, the kernel is a family of three linked constraints (network.affects_constraints). If not, the kernel is one constraint with observer-dependent classification — the framework''s ε-invariance principle would be violated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of digital_money_origin are ε-invariant distinct constraints.').

omega_variable(
    regulatory_perimeter_necessity,
    'Is the regulatory perimeter structurally necessary for monetary coordination, or does it primarily extract rents from innovators who could achieve settlement finality and stability through private order?',
    'Natural experiment: observe jurisdictions with lighter perimeters (e.g., Singapore''s stablecoin framework, Wyoming''s SPDI charter). If monetary coherence holds with lower suppression, the heavy perimeter is extractive.',
    'If the perimeter is unnecessary for coordination, this reading''s claimed tangled_rope (coordination + extraction) collapses toward snare. If necessary, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_perimeter_necessity, empirical, 'Whether the regulatory barrier is a genuine coordination requirement or an extractive moat.').

omega_variable(
    crypto_identity_lock_mechanism,
    'Is the identity_locked exit status of crypto_asset_issuers a genuine structural trap (ideological commitment makes exit impossible) or a strategic posture (exit is possible but would abandon the project''s value proposition)?',
    'Track founder/developer behavior when projects seek regulation (e.g., Circle/USDC, Paxos). If they can pivot to regulated status without abandoning their user base, the lock is strategic, not structural.',
    'If strategic, the directionality derivation overstates extraction for this seat (d should be lower). If structural, the high d is warranted and the constraint''s suppression of this seat is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crypto_identity_lock_mechanism, conceptual, 'Whether crypto issuers'' identity_locked status is structural or performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(digi_tr_t2025, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(digi_be_t2025, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(digi_su_t2025, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_origin__regulatory_recognition_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, monetary_statistics_coherence).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, payment_system_access_regime).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, central_bank_digital_currency_rollout).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three constraint stories (this reading + became_thinkable_reading + first_held_reading) because each reading authors a different ε, different beneficiary/victim structure, and different claimed type. The ε-invariance principle requires decomposition: the 'origin of digital money' label conflates conceptually distinct claims about when the constraint set became binding. This reading (regulatory recognition) has the highest suppression and latest origin; became_thinkable has the lowest suppression and earliest origin; first_held sits between. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, organized, 0.45).
constraint_indexing:directionality_override(digital_money_origin__regulatory_recognition_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
