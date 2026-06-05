% ============================================================================
% CONSTRAINT STORY: civil_rights_era_amendments__twenty_fourth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_rights_era_amendments__twenty_fourth_amendment, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: civil_rights_era_amendments__twenty_fourth_amendment
 *   human_readable: Poll Tax Abolition (Twenty-Fourth Amendment)
 *   domain: political/legal/civil_rights
 *
 * SUMMARY:
 *   The Twenty-Fourth Amendment (ratified 1964) represents a constitutional
 *   pivot point: the formal severing of voting franchise from wealth
 *   capacity. The constraint instantiated by this Amendment is the binding
 *   commitment that 'no citizen's right to vote in any primary or general
 *   election... shall be denied or abridged by reason of failure to pay any
 *   poll tax or other tax.' This reading treats the Amendment not as a
 *   historical event but as a stabilized normative kernel that commits the
 *   federal electoral system to wealth-independent suffrage. The structural
 *   delta is precise: suppression of wealth-conditioned voting mechanisms;
 *   beneficiary is poor voters across all races (especially the
 *   disenfranchised southern poor); victim set is fiscal gatekeeping regimes
 *   that relied on poll taxes as a proxy for maintaining racial and economic
 *   control over the electorate. The Amendment is a pure coordination
 *   mechanism in its aspirational form (rope) — it solves the
 *   collective-action problem of ensuring franchise equality — but from the
 *   perspective of southern gatekeeping regimes, it is a snare that
 *   forecloses their extraction tool. The extractiveness value (0.18 at
 *   stabilization) reflects that the Amendment's primary function is
 *   coordination (removing a barrier, not imposing new costs), with residual
 *   extractiveness from the regime's forced abandonment of a suppression
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Poor voters of all races: Primary beneficiary (powerless/trapped pre-amendment) — franchised by the Amendment; experience it as pure coordination
 *   - Disenfranchised southern poor: Primary beneficiary (powerless/trapped pre-amendment) — trapped by wealth barrier and racial suppression; freed by Amendment
 *   - Civil rights coalitions (NAACP, church networks, labor unions): Organized beneficiary (organized/constrained) — ratified and enforced the Amendment; coordinated voter registration and participation
 *   - Southern fiscal/administrative gatekeeping regimes: Primary victim (institutional/constrained) — loss of wealth-conditional franchise control; forced to abandon poll tax extraction mechanism
 *   - Federal electoral authority: Institutional beneficiary (institutional/arbitrage) — gains jurisdiction to enforce uniform franchise standards nationally; benefits from legitimacy of enforcing equal access
 *   - Republican national party leadership: Powerful mixed perspective (powerful/constrained) — coordination gain (access to newly enfranchised voters) alongside extraction loss (cannot use wealth-based suppression)
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing a specific reading's normative commitment as an immutable principle of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_rights_era_amendments__twenty_fourth_amendment, 0.18).
domain_priors:suppression_score(civil_rights_era_amendments__twenty_fourth_amendment, 0.52).
domain_priors:theater_ratio(civil_rights_era_amendments__twenty_fourth_amendment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fourth_amendment, extractiveness, 0.18).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fourth_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(civil_rights_era_amendments__twenty_fourth_amendment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_rights_era_amendments__twenty_fourth_amendment, rope).
narrative_ontology:human_readable(civil_rights_era_amendments__twenty_fourth_amendment, "Poll Tax Abolition (Twenty-Fourth Amendment)").
narrative_ontology:topic_domain(civil_rights_era_amendments__twenty_fourth_amendment, "political/legal/civil_rights").

domain_priors:requires_active_enforcement(civil_rights_era_amendments__twenty_fourth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(civil_rights_era_amendments__twenty_fourth_amendment, 'f7f6384e-25e8-49fa-839b-9e6850a452ad').
narrative_ontology:cs_kernel_codification('f7f6384e-25e8-49fa-839b-9e6850a452ad', formalized).
narrative_ontology:cs_authority_grounding('f7f6384e-25e8-49fa-839b-9e6850a452ad', lineage).
narrative_ontology:cs_interpretation_layer_present('f7f6384e-25e8-49fa-839b-9e6850a452ad').
narrative_ontology:cs_reading_relation('f7f6384e-25e8-49fa-839b-9e6850a452ad', civil_rights_era_amendments__twenty_fifth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('f7f6384e-25e8-49fa-839b-9e6850a452ad', civil_rights_era_amendments__twenty_sixth_amendment, influences).
narrative_ontology:cs_reading_relation('f7f6384e-25e8-49fa-839b-9e6850a452ad', civil_rights_era_amendments__twenty_third_amendment, coexists_with).
narrative_ontology:cs_axiom('f7f6384e-25e8-49fa-839b-9e6850a452ad', foundational, wealth_conditioned_voting_forbidden).
narrative_ontology:cs_axiom_status(wealth_conditioned_voting_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('f7f6384e-25e8-49fa-839b-9e6850a452ad', wealth_conditioned_voting_forbidden, deontological).
narrative_ontology:cs_axiom('f7f6384e-25e8-49fa-839b-9e6850a452ad', secondary, federal_jurisdiction_supremacy_franchise).
narrative_ontology:cs_axiom_status(federal_jurisdiction_supremacy_franchise, holdable).
narrative_ontology:cs_axiom_grounding('f7f6384e-25e8-49fa-839b-9e6850a452ad', federal_jurisdiction_supremacy_franchise, conventional).
narrative_ontology:cs_reference_frame('f7f6384e-25e8-49fa-839b-9e6850a452ad', wealth_independent_franchise).
narrative_ontology:cs_drift_state('f7f6384e-25e8-49fa-839b-9e6850a452ad', contemporary_voter_suppression_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7f6384e-25e8-49fa-839b-9e6850a452ad', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(civil_rights_era_amendments__twenty_fourth_amendment, civil_rights_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_fourth_amendment, poor_voters_all_races).
narrative_ontology:constraint_beneficiary(civil_rights_era_amendments__twenty_fourth_amendment, disenfranchised_southern_poor).
narrative_ontology:constraint_victim(civil_rights_era_amendments__twenty_fourth_amendment, fiscal_gatekeeping_regimes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED POOR (ROPE) — Before the Amendment, trapped by wealth barriers. After ratification, the constraint shifts from 'you cannot vote because you are poor' to 'you can vote, and the Amendment coordinates this with enforcement'. The poor voter experiences the Amendment as pure coordination: a collective commitment to remove the payment barrier. Effective extraction is near zero — the beneficiary experiences no cost, only access gain.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS COALITIONS (ROPE) — Organized movements (NAACP, church networks, labor unions) coordinated the ratification campaign. They experience the constraint as a coordination mechanism: mobilizing voting participation, coordinating registration drives, enforcing the Amendment's implementation against southern resistance. Extraction is low — the benefit (expanded democratic participation) is genuinely shared. The Amendment is pure coordination from this perspective.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOUTHERN GATEKEEPING REGIMES (SNARE) — Southern states' fiscal and administrative establishment relied on poll taxes as a wealth-conditioned suppression mechanism. The Amendment forecloses their extraction mechanism entirely. They experience the constraint as pure suppression: they lose a tool they were using to extract (control voting access in exchange for payment or exemption). From their structural position, the Amendment is a snare — it traps them out of their preferred governance model.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEDERAL ELECTORAL AUTHORITY (ROPE) — From the federal/national government perspective, the Amendment coordinates a single national standard for electoral access. The federal authority benefits from enforcing uniform franchise rules (arbitrage: can leverage the Amendment against state gatekeeping). Extraction is minimal — the coordination function is genuine, the enforcement mechanism is legitimate, and the federal authority gains legitimacy from enforcing equal access. Pure rope.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN ELECTORAL STRATEGY (TANGLED ROPE) — From the perspective of national Republican party leadership in the 1960s, the Amendment coordinates access (beneficiary: ability to mobilize poor and minority voters) with enforcement costs (victim: having to compete in newly expanded electorate without traditional suppression tools). The constraint is mixed: genuine coordination function (universal franchise enables broader coalition-building) alongside asymmetric extraction (forced to abandon wealth-based voter suppression, a tool they did not invent but were using). Time horizon: generational reveals that the extraction mechanism decays as new voters integrate into the electorate.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, equal access to the franchise independent of wealth is a foundational principle of democratic self-government — an immutable commitment once constitutionalized. The Amendment appears as an irreducible constraint: the idea that 'voting is conditioned on ability to pay' is structurally impossible within a framework that treats democratic participation as a fundamental right. However, the structural data contradicts the mountain classification — this Amendment is a contingent historical choice (not all democracies made this choice; many democracies elsewhere still use wealth tests). The engine will compute this as a false summit, revealing that the 'inherent to democracy' framing naturalizes what is actually a reading-specific normative commitment.
constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_rights_era_amendments__twenty_fourth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_rights_era_amendments__twenty_fourth_amendment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(civil_rights_era_amendments__twenty_fourth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18, stabilized): Low. The Amendment's primary structural function is coordination: removing a barrier (wealth requirement) from electoral access. The residual extractiveness reflects the cost to southern gatekeeping regimes (they can no longer extract control via poll taxes), not a cost imposed on beneficiaries. Base extractiveness at t=0 (pre-amendment) is coded as 0.52, reflecting the poll tax system itself — a high-extraction wealth barrier on voting. Post-ratification, extractiveness drops to 0.28 (immediate enforcement phase: ongoing state resistance, litigation, substitution attempts) and stabilizes at 0.18 (normalized federal enforcement, diminished gatekeeping capacity). The trajectory shows the Amendment's effect: it suppresses the extraction mechanism. Suppression (0.52): Moderate-high. The Amendment directly targets suppression mechanisms (poll taxes, wealth barriers). However, 0.52 reflects not the Amendment's own suppressive force but the suppressive mechanisms it abolishes — coded as 'suppression' in the pre-amendment regime. The Amendment's own suppressive capacity (enforcement against non-compliant states) is lower (≈0.35). Theater ratio (0.35): Low-moderate. The Amendment's implementation involves genuine electoral machinery (registration, enforcement) but relatively low performative content compared to, e.g., literacy tests (which are pure theater) or slow-walking implementation (which adds theater). The low theater reflects that the Amendment's coordination function is direct and measurable: voters are or are not paying poll taxes. No ambiguity in verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: from poor voters' position, the Amendment is liberatory coordination (rope); from southern gatekeeping regimes' position, the Amendment is foreclosure of their extraction mechanism (snare). The analytical observer risks naturalizing one reading as inevitable law (mountain) when it is actually a contingent constitutional choice specific to this Amendment's committer frame. The southern regime's experienced classification (snare) reveals the Amendment as successfully targeted suppression foreclosure, not just access grant. The Republican party's tangled rope classification shows that the Amendment is not purely beneficial — it coordinates access but forecloses a tool some beneficiaries had been using. The federal authority's rope classification reveals the Amendment as a genuine coordination mechanism for uniform franchise rules at the national level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the structural position of each agent relative to the wealth-condition barrier. Poor voters (pre-amendment) have d ≈ 0.95 (full targets of wealth extraction). Southern gatekeeping regimes have d ≈ 0.05 (full beneficiaries of the old system, now foreclosed). Civil rights coalitions have d ≈ 0.10 (beneficiaries of the Amendment, low extraction cost). Federal authority has d ≈ 0.20 (beneficiary of jurisdictional expansion, low personal cost). Republican party has d ≈ 0.55 (mixed position: benefits from new voters, constrained by loss of suppression tool). The engine derives d from beneficiary/victim status and exit options; beneficiaries show low d, victims show high d. The Amendment's chi values range from negative (beneficiaries experience no extraction) to positive (southern regimes experience foreclosure).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy gates (extractiveness < 0.70). The classification is stable across all perspectives: rope (coordination mechanism) is the modal type, with snare (regime perspective) and mountain (false summit at analytical level) as perspectival variants. The mandatrophy would ask: 'Is this Amendment a genuine coordination mechanism or a hidden extraction on states?' The answer is structure-dependent: it coordinates for poor voters, extracts from gatekeeping regimes, and falsely appears immutable from the civilizational view. The presheaf over all perspectives resolves the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    poll_tax_evasion_mechanisms,
    'After federal abolition, could southern states substitute alternative wealth-conditioned suppression mechanisms (literacy tests, grandfather clauses, property ownership requirements for registration) that achieve similar gatekeeping without explicit poll taxes?',
    'Historical analysis of state-level substitution: comparison of voter suppression mechanisms pre- and post-1964; identification of legal successors to poll tax suppression',
    'If substitution is widespread and effective: the Amendment''s extractiveness rises substantially (0.18 → 0.40+) because the gatekeeping regime persists under different names. If substitution fails: the Amendment successfully forecloses the suppression mechanism and extractiveness remains low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(poll_tax_evasion_mechanisms, empirical, 'Whether southern states substituted alternative wealth-conditioned suppression mechanisms').

omega_variable(
    reading_specificity_federal_only,
    'Does the Amendment''s restriction to federal elections represent a genuine constitutional limit (states retain police power over state/local elections) or a deliberate compromise that perpetuates state-level wealth conditioning?',
    'Constitutional history: did ratifiers understand ''federal elections'' as a jurisdictional boundary or as a ratification compromise? Comparison with later Twenty-Sixth Amendment (not limited by election type); analysis of state poll tax persistence post-1964',
    'If genuine jurisdictional boundary: this reading is structurally distinct from a universal franchise reading (different constraint). If compromise: this reading instantiates partial gatekeeping foreclosure, and the victim set includes ''state-level poor voters'' who remain trapped. The sibling reading (universal poll tax abolition) would foreclose this reading entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_federal_only, empirical, 'Whether federal-only restriction is a jurisdictional boundary or a deliberate compromise').

omega_variable(
    committer_frame_true_victor,
    'Who was the ultimate structural victor of this Amendment: poor voters (freed from wealth barrier), northern Republicans (electoral gain in newly enfranchised populations), federal authority (jurisdictional expansion against state gatekeeping), or civil rights movements (precedent-setting)?',
    'Electoral analysis: voter registration and participation trends post-1964; party realignment; state compliance timelines; southern resistance mechanisms',
    'This is a committer-axis question: different readings answer it differently. The poor-voter reading sees themselves as victors. The Republican reading sees constraints. The southern reading sees loss. The federal reading sees institutional gain. No single answer; the presheaf over all readings is the structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_true_victor, preference, 'Who was the ultimate structural victor of this Amendment').

omega_variable(
    twenty_sixth_amendment_foreclosure,
    'Does the Twenty-Sixth Amendment (voting age 18) foreclose or coexist with this reading? If age-18 threshold and wealth-free access are both mandatory, can a reading that accepts wealth conditioning but opposes age conditioning remain coherent?',
    'Logical analysis: Do the foundational axioms of this reading (franchise independence from wealth) logically entail or foreclose franchise independence from age? Constitutional precedent: have courts treated wealth and age as analogous suspect classifications?',
    'If foreclosure: this reading and a hypothetical ''age-conditioned franchise'' reading cannot coexist in a single interpretive framework. If coexistence: the readings occupy different parties'' commitments (some democracies condition on age, others on wealth). The sibling Twenty-Sixth Amendment reading may show different relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(twenty_sixth_amendment_foreclosure, conceptual, 'Whether Twenty-Sixth Amendment forecloses wealth-conditioning readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_rights_era_amendments__twenty_fourth_amendment, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(t24a_theater_baseline, civil_rights_era_amendments__twenty_fourth_amendment, theater_ratio, 0, 0.28).
narrative_ontology:measurement(t24a_theater_post_ratification, civil_rights_era_amendments__twenty_fourth_amendment, theater_ratio, 2, 0.32).
narrative_ontology:measurement(t24a_theater_enforcement_phase, civil_rights_era_amendments__twenty_fourth_amendment, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(t24a_extract_pre_amendment, civil_rights_era_amendments__twenty_fourth_amendment, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(t24a_extract_immediate_post, civil_rights_era_amendments__twenty_fourth_amendment, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(t24a_extract_stabilized, civil_rights_era_amendments__twenty_fourth_amendment, base_extractiveness, 5, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_rights_era_amendments__twenty_fourth_amendment, resource_allocation).
narrative_ontology:boltzmann_floor_override(civil_rights_era_amendments__twenty_fourth_amendment, 0.08).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fourth_amendment, twenty_sixth_amendment).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fourth_amendment, voting_rights_act_1965).
narrative_ontology:affects_constraint(civil_rights_era_amendments__twenty_fourth_amendment, literacy_test_bans).

% DUAL FORMULATION NOTE:
% Poll tax abolition is one reading of civil rights-era franchise expansion. Upstream: the general principle of democratic equality (uncontested, near-natural-law status). Downstream: specific substitution mechanisms (literacy tests, grandfather clauses, voter ID requirements) that southern states attempted to maintain gatekeeping after poll tax abolition. Each downstream constraint has its own extractiveness value reflecting its specific mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civil_rights_era_amendments__twenty_fourth_amendment, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
