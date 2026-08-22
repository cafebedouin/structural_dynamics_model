% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Doctrine Under Market Licensing Interpretation
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the fair use doctrine as a
 *   statutory exception to copyright. The market-licensing reading holds that
 *   fair use is foreclosed whenever a licensing market exists or could exist
 *   for the use. Under this interpretation, the doctrine becomes a residual
 *   exception for uses where no copyright holder would accept payment—uses
 *   too trivial, transformative, or culturally marginal to monetize. This
 *   reading is authored with extremely high extractiveness (0.91) because it
 *   functionally eliminates fair use as a doctrine that protects secondary
 *   creators and transforms licensed uses into universal licensing
 *   requirements. The constraint is claimed as tangled_rope because it
 *   preserves a coordination function (licensing clarity) while
 *   asymmetrically extracting from reusers who previously enjoyed fair use
 *   protections.
 *
 * KEY AGENTS:
 *   - copyright_holders: benefit from expansion of licensing opportunities and erosion of fair use defenses
 *   - licensing_intermediaries: profit from administering licensing for uses that would previously claim fair use
 *   - transformative_creators: pay licensing fees (or cease practice) for uses that embodied their creative methodology under prior fair use doctrine
 *   - non_licensed_reusers: lose access to fair use as a cost-free alternative for secondary uses
 *   - courts_applying_doctrine: enforce the market-existence test as the binding principle, subordinating traditional fair use balancing
 *   - legislative_intent_observers: dispute the reading's grounding in Congressional intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.91).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.87).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Doctrine Under Market Licensing Interpretation").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '44dd3275-e755-476f-b161-e316179c622d').
narrative_ontology:cs_kernel_codification('44dd3275-e755-476f-b161-e316179c622d', fixed_text).
narrative_ontology:cs_authority_grounding('44dd3275-e755-476f-b161-e316179c622d', extraction).
narrative_ontology:cs_interpretation_layer_present('44dd3275-e755-476f-b161-e316179c622d').
narrative_ontology:cs_reading_relation('44dd3275-e755-476f-b161-e316179c622d', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('44dd3275-e755-476f-b161-e316179c622d', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('44dd3275-e755-476f-b161-e316179c622d', foundational, licensing_market_protection_overrides_transformation).
narrative_ontology:cs_axiom_status(licensing_market_protection_overrides_transformation, holdable).
narrative_ontology:cs_axiom_grounding('44dd3275-e755-476f-b161-e316179c622d', licensing_market_protection_overrides_transformation, instrumental).
narrative_ontology:cs_axiom('44dd3275-e755-476f-b161-e316179c622d', foundational, fair_use_exists_only_in_market_voids).
narrative_ontology:cs_axiom_status(fair_use_exists_only_in_market_voids, holdable).
narrative_ontology:cs_axiom_grounding('44dd3275-e755-476f-b161-e316179c622d', fair_use_exists_only_in_market_voids, deontological).
narrative_ontology:cs_reference_frame('44dd3275-e755-476f-b161-e316179c622d', copyright_as_property_licensing_foundation).
narrative_ontology:cs_drift_state('44dd3275-e755-476f-b161-e316179c622d', contemporary_secondary_creator_practice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44dd3275-e755-476f-b161-e316179c622d', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, non_licensed_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control reproduction and distribution of protected works. Under the market-licensing reading, every potential reuse that could be monetized (i.e., every substantial reuse) becomes a licensing opportunity. Retain exclusive control over derivative markets and transformative adaptation. Receive licensing fees from reusers who seek permission rather than claiming fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Administer rights clearance infrastructure, licensing portals, and permission-granting services. The market-licensing reading expands their addressable market by converting discretionary uses (that would otherwise claim fair use) into mandatory-licensing transactions. Profit from transaction fees, licensing administration, and rights management systems.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries, beneficiary,
    institutional, generational, mobile, global).

% Seek to use copyrighted material for secondary purposes: quotation, criticism, adaptation, remix, educational use, or commentary. Under the market-licensing reading, they face a binary choice: pay for licensing or cease the use. Fair use claims are foreclosed by the logic that any use with a licensing market is not fair use. They lack the resources or standing to negotiate directly with copyright holders in most cases.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, non_licensed_reusers, payer,
    moderate, biographical, constrained, global).

% Artists, scholars, and creators whose practice inherently involves reusing, sampling, quoting, or transforming existing works as part of cultural production. Under the market-licensing reading, transformation itself becomes a licensing trigger. Their creative identity and methodology is built on practices the reading reclassifies as compensable licensing events rather than fair use. They are effectively excluded from the doctrine by its expansion into the transformative domain.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_creators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, transformative_creators, excluded).

% Seek to preserve a robust public domain and unrestricted reuse zone. They argue that fair use should be a meaningful doctrine that enables uses that copyright markets do not serve. The market-licensing reading eliminates that argument: any use where a licensing market could exist is deemed to harm that market and thus forfeit fair use. They lose standing to claim fair use defense as the doctrine collapses into market-existence test.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, payer,
    moderate, generational, constrained, global).

% Adjudicate fair use claims under 17 U.S.C. § 107. This reading instructs courts that the market-licensing principle overrides the fair use doctrine: any use that could be licensed harms the market for that license, so fair use protection shrinks to uses where no licensing market exists or could exist. Courts become enforcers of market-expansion logic rather than balancers of competing interests.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_applying_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% Interpret Congressional intent in enacting the fair use doctrine as an equitable limit on copyright exclusivity. This reading claims to ground itself in Congressional will to protect licensing markets. Other readings (transformative_right, narrow_defense) dispute this interpretation. The observer seat sees the reading's legitimacy claims but has no enforcement power over doctrine application.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, legislative_intent_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes licensing decisions and revenue collection through a single interpretive principle: that fair use exists only in market voids. This coordination claim asserts that licensing markets are more efficient than fair-use-case-by-case balancing, and that doctrine should maximize monetization of reuse rather than preserving an unmonitized public domain.
% TRANSFER_FUNCTION: Moves reuse licensing fees from secondary creators and users to copyright holders and licensing intermediaries. Under this reading, uses that would previously claim fair use become paid licensing events, transferring what was a free right into a compensable transfer.
% ABSENT_VOICES: Transformative creators, remix artists, and scholars whose practice relies on fair use are effectively excluded from the interpretive conversation because the reading's own logic forecloses their claim. Legislative intent around cultural production and educational benefit is subordinated to licensing-market preservation. International fair-use traditions emphasizing transformative benefit remain unheard in this U.S.-centric reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished and fair use reverted to the transformative-right or narrow-defense framings, the landscape would reorganize: secondary creators would recover fair use claims in cases the market-licensing reading currently forecloses; licensing demand would drop as users reassert fair-use defenses; copyright holders would lose licensing revenue from uses they currently monetize; remix and transformative culture would resume practices the reading had suppressed.
% FOUNDING_PROBLEM: Copyright markets require incentive protection to function. Uncontrolled secondary use erodes the value of licensing markets and makes copyright less valuable as an incentive structure. Fair use, understood as preserving un-licensed reuse, becomes incompatible with market-based incentive theory.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing-industry organizations attest that the founding problem is live and that licensing-market protection is essential to copyright economics. Economists from outside the copyright industry and fair-use scholars dispute both claims: secondary licensing is a small fraction of copyright value (Harper & Row Publishers, Inc. v. Nation Enterprises, 471 U.S. 539 (1985), established that harm to licensing markets is only one factor, not a veto); transformative secondary uses create value that licensing markets do not capture; empirical studies show fair use and cultural reuse drive downstream copyright demand. Legislative history does not endorse the market-licensing reading as the test; Congress explicitly preserved fair use as an equitable doctrine and did not instruct courts to subordinate transformation to licensing-market protection.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.91) because the reading functionally eliminates fair use as a protective doctrine for secondary creators. Under the market-licensing principle, any use that *could* be licensed—which encompasses virtually all substantial reuse—loses fair use protection by definition. The reading thus converts a discretionary exception (balanced case-by-case) into a licensing requirement (pay or forbear). Suppression is high (0.87) because enforcing this reading requires courts to reject fair use claims, constrain the doctrine's protective scope, and suppress the countervailing principle that fair use enables transformative cultural production. Theater (0.42) is moderate because the reading maintains a facade of fair use doctrine (cases still exist where licensing markets do not exist), but the practical doctrine has atrophied—fair use claims are foreclosed by the market-existence test before reaching traditional balancing. The measurement series shows rising extractiveness and suppression over the interval (0–40) as the reading's application accumulates case law, narrowing fair use's scope and expanding licensing's reach. Rising theater reflects increasing performance of fair-use application in cases where the outcome is predetermined by market existence.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder and licensing-intermediary seats experience this reading as efficient coordination: a clear rule that maximizes licensing markets and provides certainty. The transformative-creator and fair-use-advocate seats experience it as foreclosure: the doctrine they depended on is eliminated by a tautology (any use that could be licensed harms the licensing market). From the court's position, applying the reading is rule-like (ask: does a licensing market exist?) and avoids case-by-case balancing. From the secondary creator's position, the reading is a structural trap: any use worth protecting loses protection because worth-protecting uses have licensing markets. The engine computes per-seat classification divergence from this asymmetry: copyright holders and intermediaries compute toward beneficiary directionality (d near 0.0), reusers compute toward target directionality (d near 1.0), courts compute as enforcers of an institutional principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are structural beneficiaries: they control the licensing system, collect licensing fees from uses that would previously be free, and benefit from the expansion of monetizable reuse. Licensing intermediaries are beneficiaries: they profit from administering licensing for expanding numbers of uses. Transformative creators are structural victims: uses that embody their practice (sampling, quotation, adaptation, remix) are reclassified as licensing events. Their identity-locked exit (creative practice is built on these uses) makes them trapped targets. Non-licensed reusers are victims: they lose the fair use defense by definition. Public-domain advocates are victims: they lose the argument that fair use preserves an unmonitized reuse zone. Courts are agenda-setters: they apply and reinforce the reading. The directionality chain yields high effective extraction (χ) for the victim seats because they are targets (d near 1.0) at institutional scope, with constrained exit options (licensing or cessation) and no negotiating power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the market-licensing reading is that licensing markets require protection from fair use to function as incentives. However, this framing contains its own obsolescence: if fair use had collapsed as thoroughly as the reading mandates, copyright markets would exist and expand indefinitely, but secondary creative production would atrophy. The reading survives not because the founding problem remains live, but because copyright holders actively enforce the reading's application—making it a snare rather than a mandatrophy-resolved tangled_rope. The constraint avoids mandatrophy because its beneficiaries (copyright holders and intermediaries) continue to police its boundaries through litigation and licensing demands. The divergence between the reading's coordinate function (clear rules for licensing) and its extractive operation (blanket suppression of fair use claims) marks it as increasingly hollow: courts and the copyright industry treat it as a binding interpretive principle, but its functional purpose (incentivizing creation through copyright) is decoupled from the mechanism (suppressing secondary use). The reading persists through theater: licensing-market protection is presented as copyright theory, but the practice is pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_market_existence_boundary,
    'What constitutes the existence of a ''licensing market'' for fair use doctrine purposes? Is potential licensing demand (a market that could exist) treated identically to actual licensing demand (a market that does exist)?',
    'Courts clarifying whether the market test applies to hypothetical licensing markets (any use that COULD be licensed) or only to existing, functioning markets (uses already actively licensed). Legislative guidance or revised statutory language could establish this boundary.',
    'If the reading is interpreted to apply the test to potential markets only, virtually all substantial reuse is foreclosed from fair use. If it applies to functioning markets only, some fair use space remains for uses where no actual licensing infrastructure exists. This distinction determines whether fair use collapses entirely or retains a residual domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_existence_boundary, conceptual, 'Whether the market-licensing test applies to potential or actual licensing markets.').

omega_variable(
    transformative_use_exception_within_reading,
    'Does the market-licensing reading preserve any space for transformative uses that might happen to exist within licensing markets, or does transformation itself lose fair use status whenever licensing is possible?',
    'Court decisions clarifying whether transformativeness is an independent fair use factor or whether market-licensing drowns out all other factors. Comparative analysis of jurisdictions where transformative reuse receives explicit statutory protection.',
    'If transformative uses retain fair use protection within licensing markets, the reading is less extractive than authored (0.91 might drop to 0.75–0.80). If transformation loses protection by definition whenever licensing exists, the reading is purely extractive and fair use doctrine is functionally dead for secondary creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_exception_within_reading, empirical, 'Whether transformation survives as a fair use principle under the market-licensing reading.').

omega_variable(
    congressional_intent_vs_reading,
    'What was Congress''s actual intent in preserving fair use as an equitable exception in 1976? Does the legislative record support the market-licensing reading as an interpretation of fair use, or does the record support the transformative-right or narrow-defense readings instead?',
    'Legislative history research, statutory purpose analysis, and comparative reading of the fair use doctrine''s origins in equity and the doctrine''s placement as a broad exception rather than a narrow market-protective carve-out.',
    'If legislative intent does not support the market-licensing reading, the reading loses its grounding claim (that it implements Congressional will to protect licensing markets) and becomes vulnerable to reclassification as a snare or overreach. This is a gate on whether the reading''s legitimacy claim stands or collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_intent_vs_reading, empirical, 'Does the market-licensing reading rest on Congressional intent, or does the legislative record contradict it?').

omega_variable(
    secondary_creator_exit_trajectories,
    'When transformative creators and secondary users lose fair use protection under this reading, do they exit their practices entirely, or do they develop adaptation strategies (licensing, circumvention, international migration, or alternative creation methods) that preserve their creative identity?',
    'Empirical study of creator behavior post-reading adoption: exit rates, licensing adoption, practice reformulation, jurisdictional migration. Post-exit trajectory analysis: do suppression effects persist after creators relocate or cease the practice?',
    'If creators cease transformative practice, the reading achieves suppression but at the cost of cultural production and atrophies the secondary-creation ecosystem that fair use was designed to protect. If they adapt by licensing, the reading succeeds but at higher transaction costs. If they migrate or circumvent, the reading may increase inefficiency without reducing secondary use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secondary_creator_exit_trajectories, empirical, 'Do secondary creators exit the doctrine, adapt through licensing, or develop workarounds when fair use protection collapses?').

omega_variable(
    internalized_suppression_licensing_model,
    'Is the suppression required to enforce the market-licensing reading primarily structural (courts rejecting fair use claims, institutional licensing requirements) or partially internalized (secondary creators self-suppress, believing licensing is mandatory even where fair use would apply)?',
    'Survey and interview data from transformative creators about their understanding of fair use obligations; behavior-change analysis comparing pre- and post-reading adoption; study of licensing adoption rates relative to fair use claim potential.',
    'If suppression is largely internalized (creators believe they must license even where fair use applies), the reading''s effective extraction is higher than the structural suppression metric suggests—the creators carry the suppression with them even if the legal rule is later relaxed. If suppression is purely structural (courts enforce it; creators would claim fair use if the rule were clearer), relaxing the rule might recover fair use practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_licensing_model, empirical, 'What portion of the measured suppression (0.87) is structural enforcement versus internalized belief?').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of the fair use kernel. What evidence would establish that the market-licensing reading is the correct interpretation of Congressional intent, versus the transformative-right or narrow-defense readings?',
    'This is a structural question routed through committer-frame analysis: it names the irreducible disagreement between readings. Resolution would require a court decision, legislative clarification, or demonstrated empirical mismatch between the reading''s predictions and actual copyright-incentive effects. No single empirical fact resolves it—the readings rest on different normative foundations (market protection vs. cultural enablement vs. property boundaries).',
    'If the market-licensing reading is established as the binding interpretation, transformative-right reading''s claim to fair use protection is foreclosed. If transformative-right is established, the market-licensing reading is overridden. The three readings coexist currently because courts have not settled the interpretive question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Which reading correctly interprets fair use doctrine: market-licensing, transformative-right, or narrow-defense?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t5, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(fair_tr_t5, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t15, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(fair_tr_t15, observed).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(fair_tr_t20, observed).
narrative_ontology:measurement(fair_tr_t25, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(fair_tr_t25, observed).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(fair_tr_t30, observed).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(fair_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t5, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 5, 0.73).
narrative_ontology:measurement_basis(fair_be_t5, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t15, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement_basis(fair_be_t15, observed).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(fair_be_t20, observed).
narrative_ontology:measurement(fair_be_t25, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 25, 0.89).
narrative_ontology:measurement_basis(fair_be_t25, observed).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 30, 0.9).
narrative_ontology:measurement_basis(fair_be_t30, observed).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.91).
narrative_ontology:measurement_basis(fair_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t5, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement_basis(fair_su_t5, observed).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(fair_su_t10, observed).
narrative_ontology:measurement(fair_su_t15, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement_basis(fair_su_t15, observed).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement_basis(fair_su_t20, observed).
narrative_ontology:measurement(fair_su_t25, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement_basis(fair_su_t25, observed).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement_basis(fair_su_t30, observed).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement_basis(fair_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_licensing_markets__infrastructure).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, secondary_creator_practice__institutional_constraints).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family decomposing the fair use doctrine kernel. Each reading instantiates a distinct constraint with a different ε and type because each reading interprets the statutory text to produce structurally different effects. The market_licensing_reading (this story) produces extremely high extraction (ε=0.91) and functional foreclosure of fair use. The transformative_right_reading produces moderate extraction (fair use preserved for transformative uses) and coordination function (cultural enablement). The narrow_defense_reading produces moderate extraction (fair use limited but not eliminated) and coordination (property-based incentive preservation). The three readings do not represent measurement-basis variation on a single constraint—they represent genuinely different constraints instantiated by different interpretations of the same statutory kernel. They are linked via network.affects_constraints because the interpretive choice in one reading directly influences the scope and legitimacy of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
