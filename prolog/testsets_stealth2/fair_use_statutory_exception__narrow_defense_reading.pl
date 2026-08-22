% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property Reading)
 *   domain: intellectual_property/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the fair use kernel: the
 *   narrow_defense_reading, under which copyright is treated as excludable
 *   property, fair use operates as an affirmative defense the user must
 *   prove, commercial purpose weighs heavily against a finding of fairness,
 *   and transformativeness is subordinated to a market-harm inquiry centered
 *   on the fourth statutory factor. The standing arrangement under contest is
 *   the doctrine as administered on these premises: presumption against the
 *   user, burden on the defendant, licensing as the priced alternative to
 *   permission-by-litigation. Per the epsilon-invariance principle this file
 *   authors one stable epsilon for that arrangement; the
 *   transformative_right_reading and market_licensing_reading are separate
 *   files with their own victim sets and epsilon values, linked through
 *   network edges. The claim/metrics split is deliberate: the constraint is
 *   CLAIMED as tangled_rope (it retains a real incentive-coordination core)
 *   while the authored metrics describe substantially extractive operation.
 *   The engine measures that divergence; the author does not reconcile it.
 *   KEY AGENTS (by structural relationship): - copyright_owner_industries:
 *   Primary beneficiary (institutional/arbitrage) — collects license fees,
 *   damages, and settlements; litigates to entrench the reading -
 *   federal_courts_appellate: Agenda setter (institutional/analytical) —
 *   administers the statute, fixes burden and factor weights -
 *   licensing_intermediaries: Secondary beneficiary (organized/mobile) —
 *   commissions on every forced clearance - commercial_secondary_creators:
 *   Primary target (powerful/constrained) — pays clearance premiums and
 *   litigation risk - grassroots_remix_communities: Primary target
 *   (powerless/identity_locked) — bears the defense's full force without
 *   resources - digital_archivists_libraries: Secondary target
 *   (organized/constrained) — preservation pathway narrows with each ruling -
 *   downstream_audiences: Incidental beneficiary and diffuse cost bearer
 *   (organized/constrained) - cultural_commons_advocates: Excluded voice
 *   (organized/constrained) — heard only as amici - ip_law_scholars:
 *   Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.72).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.68).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '09d67d34-57b7-4250-88b6-d4814aa97524').
narrative_ontology:cs_kernel_codification('09d67d34-57b7-4250-88b6-d4814aa97524', fixed_text).
narrative_ontology:cs_authority_grounding('09d67d34-57b7-4250-88b6-d4814aa97524', lineage).
narrative_ontology:cs_interpretation_layer_present('09d67d34-57b7-4250-88b6-d4814aa97524').
narrative_ontology:cs_reading_relation('09d67d34-57b7-4250-88b6-d4814aa97524', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('09d67d34-57b7-4250-88b6-d4814aa97524', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('09d67d34-57b7-4250-88b6-d4814aa97524', foundational, copyright_is_excludable_property).
narrative_ontology:cs_axiom_status(copyright_is_excludable_property, holdable).
narrative_ontology:cs_axiom_grounding('09d67d34-57b7-4250-88b6-d4814aa97524', copyright_is_excludable_property, conventional).
narrative_ontology:cs_axiom('09d67d34-57b7-4250-88b6-d4814aa97524', foundational, defendant_bears_fair_use_burden).
narrative_ontology:cs_axiom_status(defendant_bears_fair_use_burden, holdable).
narrative_ontology:cs_axiom_grounding('09d67d34-57b7-4250-88b6-d4814aa97524', defendant_bears_fair_use_burden, conventional).
narrative_ontology:cs_axiom('09d67d34-57b7-4250-88b6-d4814aa97524', secondary, market_harm_factor_dominant).
narrative_ontology:cs_axiom_status(market_harm_factor_dominant, holdable).
narrative_ontology:cs_axiom_grounding('09d67d34-57b7-4250-88b6-d4814aa97524', market_harm_factor_dominant, empirically_contingent).
narrative_ontology:cs_reference_frame('09d67d34-57b7-4250-88b6-d4814aa97524', property_conception_market_guard).
narrative_ontology:cs_drift_state('09d67d34-57b7-4250-88b6-d4814aa97524', contemporary_ai_training_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('09d67d34-57b7-4250-88b6-d4814aa97524', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_owner_industries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, commercial_secondary_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, grassroots_remix_communities).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, digital_archivists_libraries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the statutory text and the case line beneath it, deciding which uses escape liability and who must prove what. Their precedents fix the burden of proof, the weight each factor carries, and how readily commercial purpose defeats a use. They collect no fees and pay none; what rides on their choices is doctrinal coherence and the distribution of litigation risk across every future dispute.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, federal_courts_appellate, agenda_setter,
    institutional, generational, analytical, national).

% Publishers, studios, labels, and estates holding large catalogs. They bring infringement actions, fund amicus campaigns, and press for longer terms and stronger remedies. License fees, statutory damages, and settlement payments flow to them; each finding against a user converts a free practice into a controlled market. Their catalog businesses can pivot formats and channels, so adverse doctrinal shifts raise costs rather than threaten survival.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_owner_industries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, copyright_owner_industries, agenda_setter).

% Collective rights organizations, stock-content agencies, and clearance houses. They take a commission on every authorized reuse and maintain the price lists and rights databases that make clearance possible at all. Their revenue scales with how much reuse must be licensed, so the breadth of the defense determines the size of their addressable market.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Documentary filmmakers, search and AI developers, and media companies whose products build on existing works. They can pay for licenses but face fragmented ownership, holdouts, and pricing set against their dependence. Every project budgets for injunction and damages exposure; some abandon whole segments rather than attempt clearance.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, commercial_secondary_creators, payer,
    powerful, biographical, constrained, global).

% Fan editors, sample-based musicians, video essayists, and meme makers operating without counsel or clearance budgets. Clearance costs exceed their production budgets many times over, and the practice is constitutive of who they are as creators. The realistic options are publishing anyway and risking takedown, or self-censoring; leaving the practice means leaving the community's craft.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, grassroots_remix_communities, payer,
    powerless, immediate, identity_locked, global).

% Libraries, archives, and preservation projects digitizing orphan and fragile works. The defense is their principal lawful pathway for access copies; each narrowing multiplies per-item legal review and insurance cost. Their missions bind them to the material, so stepping back means leaving works unpreserved rather than changing professions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, digital_archivists_libraries, payer,
    organized, generational, constrained, national).

% Readers, viewers, listeners, and the future creators they become. They receive the new works that exclusivity finances, and they also inherit the reuse gaps: parodies withdrawn, documentaries recut, archival material locked away. Where clearance costs pass through, they pay higher prices; where uses are blocked outright, they never see the work at all.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences, payer).

% Public-domain expansionists, open-access campaigners, and digital-rights organizations. They would argue the defense should operate as a user right measured by cultural productivity rather than a shield granted sparingly. They appear in the process only as amici and commentators; no party's pleadings depend on them, and no negotiation table holds a seat for them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, cultural_commons_advocates, excluded,
    organized, biographical, constrained, global).

% Academic commentators tracking the doctrine's movement across decades. They publish empirical studies of clearance costs and enforcement patterns, critique factor inflation, and maintain the historical record of the 1976 codification. They decide nothing and bear nothing directly; their stake is the accuracy of the doctrinal record.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, ip_law_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_owner_industries).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the financing problem of expressive production: exclusive rights plus a predictable, narrow escape valve give publishers and studios bankable revenue streams and give licensors a stable price list; a defense that rarely surprises reduces valuation uncertainty for catalog-backed investment.
% TRANSFER_FUNCTION: Moves money (license fees, statutory damages, settlements) and control (approval over derivative forms) from unauthorized users and would-be users to rights holders and their licensing agents; moves legal risk onto defendants through the burden allocation; and removes certain reuse opportunities from circulation altogether.
% ABSENT_VOICES: Audiences, future creators, and commons advocates hold no party seat: the adversarial frame admits only a rights holder and an accused user, so arguments about aggregate cultural production enter only when a litigant happens to carry them, usually as amici. Orphan-work users unable to identify an owner are absent even from the licensing conversation, since there is no one to negotiate with.
% DISAPPEARANCE_RATIONALE: If the narrow-defense administration vanished overnight and fair use became a broadly available user right with no defendant burden, clearance markets would reprice sharply, takedown and damages volume would collapse, remix and archival publication would surge, and catalog valuations built on controlled-derivative income would fall; the surrounding licensing economy reorganizes around the defense's breadth.
% FOUNDING_PROBLEM: The 1976 codification answered decades of judicial patchwork: Congress needed to preserve copyright's incentive function against unauthorized exploitation while leaving breathing room for criticism, comment, scholarship, and news reporting. The narrow reading specifically answers the problem of market-value erosion when second users free-ride on the first user's investment.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders attest the problem is live, pointing to streaming-era licensing markets and AI training-data demands. Corroboration from outside the beneficiary set: the empirical economics literature on copyright incentives (finding weak marginal-production effects for large legacy catalogs), library and archive association filings documenting clearance-cost barriers, and the courts' own acknowledgments across the transformative-era opinions that the defense serves purposes beyond market protection. No neutral attestation settles the contest; the documented state is the split itself.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the reading prices most unauthorized reuse: the defendant's burden, the commercial-purpose discount, and fourth-factor primacy convert would-be users into licensees or litigants, and statutory damages make the threat credible far beyond demonstrated harm. Suppression (0.68) is structural first — burden allocation, takedown leverage, and treaty limits on new exceptions — with an internalized component among remix communities who normalize self-censorship; the omega battery carries the structural/internalized split, and suppression is authored as a raw structural property, unscaled by power or scope. Theater ratio (0.38) reflects a four-factor recital that frequently tracks a predetermined market-harm conclusion: the factors are announced, the fourth decides. Accessibility collapse is moderate (0.50) because exits persist — licensing, open licenses, waiting out terms — but each is costly or slow. Resistance (0.60) is real: scholarly critique, amicus campaigns, and the sibling readings themselves are the organized form of that resistance. The measurement series run on one shared grid (1976-2025, seven points, all three tracked metrics authored at every point). The series oscillate rather than drift monotonically: the 1984 Sony aftermath and Campbell (1994) depress extraction and enforcement demand; DMCA-era consolidation and Eldred (2003) restore them; the transformative-era peak eases them again; Warhol (2023) and the AI licensing push drive the current crest. The oscillation is driven by doctrinal coalition turnover rather than an intermittent-reinforcement design, though rights-holder litigation strategy times filings to favorable swings. Scalars are authored at the 2025 endpoint, the revival crest.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the copyright_owner_industries seat the arrangement is the coordination they finance: exclusivity funds production, and a narrow defense protects the markets that fund the next work — close to a rope with modest overhead. From the grassroots_remix_communities and digital_archivists_libraries seats the same structure operates as a tollgate with the burden reversed — closer to enforced extraction wearing a coordination veneer. Commercial_secondary_creators straddle: they can buy their way through, which changes the experienced type without changing the underlying asymmetry. The appellate bench experiences neither pole; it administers a balancing test whose outputs differ by circuit and era. The engine computes these per-seat types from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: copyright_owner_industries receives the transfers directly and holds arbitrage-grade adaptation, pushing d toward 0; licensing_intermediaries collect a commission on every forced clearance, similarly low d. Targets sit near the full-target end: grassroots_remix_communities combine maximal cost incidence with identity-locked exit (the practice is who they are), placing them nearest d = 1.0; digital_archivists_libraries are mission-trapped with comparably high d; commercial_secondary_creators bear high d but retain partial mobility through budgeting and deal-making, moderating effective extraction. Downstream_audiences derive real benefit from financed production while absorbing diffuse costs, landing near symmetric with a mild target tilt through their secondary payer position. The bench is analytical and collects nothing. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already produce the correct spread.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope keeps both halves visible. A pure-snare reading would erase the genuine incentive function — exclusivity demonstrably finances production, and the licensing apparatus solves a real clearance problem; a pure-rope reading would erase the asymmetry — burden allocation and fourth-factor primacy concentrate costs on precisely the actors with the least exit. The R5 interview shows no zombie signature: founding_problem_status is contested (not dead) and disappearance_verdict is world_rearranges, so the mismatch consumer finds no dead-mandate/world-rearranges collision. Theater_ratio at 0.38 stays well short of performative-maintenance territory, and enforcement is real rather than ceremonial. Mandatrophy resolution therefore turns on the live contest over whether market-value protection remains the doctrine's function or has become its cover — carried as omegas rather than resolved by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the fair_use_statutory_exception kernel, the narrow_defense_reading. Which structural features would change if a sibling reading were adopted instead?',
    'Comparative classification across the three sibling files (transformative_right_reading, market_licensing_reading): differences in victim sets, burden allocation, and factor weighting locate the disagreement structurally.',
    'Under transformative_right_reading, epsilon drops for non-commercial transformative uses and the victim set shrinks toward uncompensated rights holders; under market_licensing_reading, epsilon rises further and the victim set expands to any use with a hypothetical licensing market.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed commitment: this story instantiates the narrow defense reading of the fair use kernel; sibling readings are separate constraints with their own epsilon.').

omega_variable(
    market_harm_counterfactual_status,
    'Is market harm in the fourth factor an observable injury or an untestable counterfactual asserted by rights holders?',
    'Licensing-market data around takedown waves and refusal letters; difference-in-differences comparisons of works subject to enforcement versus comparable unenforced works.',
    'If harm is largely counterfactual, the reading''s operative test is unfalsifiable and its true reach exceeds what litigation outcomes reveal; if measurable, the narrow reading gains empirical footing and its extraction estimate stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_counterfactual_status, empirical, 'Whether the fourth-factor inquiry tracks verifiable market substitution or assertion.').

omega_variable(
    burden_allocation_chilling_effect,
    'How much defensible reuse never reaches adjudication because the defendant''s burden deters anyone from relying on the defense?',
    'Surveys of abandoned or recut publications before and after burden-shifting precedents; clearance-cost audits across documentary and archival sectors.',
    'Heavy chilling means extraction measured from litigated cases badly understates the reading''s reach; classification should then weight suppression above litigation-derived estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_allocation_chilling_effect, empirical, 'Invisible deterrence: the gap between challenged uses and deterred uses under a defendant-side burden.').

omega_variable(
    ai_training_market_expansion,
    'Does AI training create a cognizable licensing market such that the narrow reading extends to training corpora, or is training non-rival transformative use outside the market-harm frame?',
    'Outcomes of pending litigation and emergent licensing deals; economic analysis of substitutability between training data and licensed outputs.',
    'If training counts as a market, the reading''s extraction surface expands massively and a new victim class (model developers and the information commons) enters the structure; if not, the reading contracts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_market_expansion, empirical, 'The live frontier where the narrow reading''s scope will be decided.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fu_narrow_defense_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(fu_narrow_defense_tr_t1989, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1989, 0.24).
narrative_ontology:measurement(fu_narrow_defense_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement(fu_narrow_defense_tr_t2003, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(fu_narrow_defense_tr_t2013, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(fu_narrow_defense_tr_t2023, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement(fu_narrow_defense_tr_t2025, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(fu_narrow_defense_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(fu_narrow_defense_be_t1989, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1989, 0.52).
narrative_ontology:measurement(fu_narrow_defense_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.48).
narrative_ontology:measurement(fu_narrow_defense_be_t2003, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(fu_narrow_defense_be_t2013, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2013, 0.54).
narrative_ontology:measurement(fu_narrow_defense_be_t2023, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(fu_narrow_defense_be_t2025, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fu_narrow_defense_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(fu_narrow_defense_su_t1989, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1989, 0.46).
narrative_ontology:measurement(fu_narrow_defense_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.44).
narrative_ontology:measurement(fu_narrow_defense_su_t2003, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2003, 0.6).
narrative_ontology:measurement(fu_narrow_defense_su_t2013, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2013, 0.56).
narrative_ontology:measurement(fu_narrow_defense_su_t2023, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2023, 0.68).
narrative_ontology:measurement(fu_narrow_defense_su_t2025, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file instantiates the narrow_defense_reading (high epsilon, defendant's burden, market-value preservation as organizing purpose). transformative_right_reading and market_licensing_reading instantiate sibling arrangements with different victim sets and epsilon values. Structural relations: the narrow reading's fourth-factor primacy feeds the licensing reading's expansion (influences), while the transformative reading competes across judicial coalitions without logical elimination on either side (coexists_with). Upstream/downstream: whichever reading controls burden allocation and factor weighting conditions the operating environment of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
