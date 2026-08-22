% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Absolute Sovereign Discretion over Admission (Border Control as Constitutive of Statehood)
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This file instantiates the sovereignty_primary reading of the
 *   border_control_legitimacy kernel: the claim that state territorial
 *   sovereignty entails absolute discretion to exclude non-citizens, with
 *   border control constitutive of statehood itself. On this reading, the
 *   standing global border-control arrangement — visa regimes, patrol and
 *   interdiction, detention and removal, externalized processing — is the
 *   defense of a constitutive sovereign function, and human-rights limits sit
 *   outside legitimate authority rather than inside it. The authored metrics
 *   describe the arrangement's actual operation independently of that claim:
 *   heavy coercive machinery, rising performative enforcement, persistent
 *   resistance, and a cost side that even this reading's own lights must
 *   partly count as beyond-defense excess. Constraint family: siblings
 *   freedom_of_movement_primary and jurisdictional_sovereignty are separate
 *   files over the same referent with their own epsilon, victim sets, and
 *   types, linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - territorial_states: Agenda setter
 *   (institutional/arbitrage) — administers and enforces the exclusion
 *   regime; collects fees, fines, and the political good of asserted control
 *   - citizen_majorities: Primary beneficiary (organized/constrained) —
 *   receive bounded membership, labor protection, and the welfare perimeter -
 *   excluded_would_be_migrants: Primary target (powerless/trapped) — bear
 *   interception, detention, removal, and foreclosed lawful entry -
 *   asylum_seekers: Target subset (powerless/trapped) — flight-based
 *   claimants intercepted before territory - border_enforcement_industry:
 *   Secondary beneficiary (organized/mobile) — collects procurement spending;
 *   lobbies for scale - employers_of_deportable_labor: Secondary beneficiary
 *   (powerful/mobile) — harvest the right-less workforce the regime's
 *   strictness produces - human_rights_bodies: Excluded critic
 *   (institutional/trapped) — denied standing inside the legitimacy frame;
 *   presses externally - international_law_scholars: Analytical observer
 *   (analytical/analytical) — tests the constitutive claim against historical
 *   counterexample
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.32).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.8).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.32).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Absolute Sovereign Discretion over Admission (Border Control as Constitutive of Statehood)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd').
narrative_ontology:cs_kernel_codification('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', formalized).
narrative_ontology:cs_authority_grounding('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', lineage).
narrative_ontology:cs_interpretation_layer_present('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd').
narrative_ontology:cs_reading_relation('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', border_control_legitimacy__jurisdictional_sovereignty, forecloses).
narrative_ontology:cs_axiom('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', foundational, admission_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(admission_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', admission_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', foundational, exclusion_prerogative_self_justifying).
narrative_ontology:cs_axiom_status(exclusion_prerogative_self_justifying, holdable).
narrative_ontology:cs_axiom_grounding('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', exclusion_prerogative_self_justifying, deontological).
narrative_ontology:cs_reference_frame('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', westphalian_exclusive_admission_discretion).
narrative_ontology:cs_drift_state('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7acb4a68-59c3-4099-aa3e-7d95f7bf1bfd', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, territorial_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_majorities).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, employers_of_deportable_labor).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, employers_of_deportable_labor).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, constitutive_border_control_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains visa regimes, patrols land and sea borders, runs detention and removal systems, and asserts final authority over who crosses. Justifies each instrument as defense of a constitutive sovereign function rather than a policy choice among alternatives. Collects visa fees, carrier fines, and the domestic political credit of asserted control; funds the apparatus from general revenue. Exit is effectively unlimited: the state writes the rules it enforces and can recalibrate them at will.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, territorial_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds membership in the bounded community the regime maintains: preferential labor-market position, a welfare perimeter, and a shared political identity defined partly against outsiders. Bears the fiscal cost of enforcement through taxation, occasional labor shortages in restricted sectors, and reputational costs abroad. Emigration is possible in principle but severs citizenship ties, family networks, and pension claims, so most never treat it as a live option.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_majorities, beneficiary,
    organized, biographical, constrained, national).

% Seeks entry for work, family reunification, or safety and finds lawful channels closed or rationed far below demand. Bears interception at sea and land, detention, removal, and — where lawful routes fail — the mortal risks of smuggled passage. Has no procedural standing in the states whose discretion governs them; their only levers are physical persistence and collective movement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_would_be_migrants, payer,
    powerless, immediate, trapped, regional).

% Flees persecution and is formally entitled to lodge a claim, but externalized checkpoints, safe-third-country rules, and pushback practices intercept them before territory is reached. Cannot return home without facing the danger they fled, cannot advance without crossing, and waits years in transit states for processes they cannot trigger.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, regional).

% Supplies detention beds, surveillance towers, biometric databases, patrol vessels, and processing software under government contract. Revenue scales with enforcement budgets, so the industry lobbies for expansion, markets threat scenarios to legislators, and diversifies into adjacent homeland-security lines when one market tightens.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, agenda_setter).

% Operates agriculture, construction, care, and hospitality workforces that include workers without secure status. The regime's strictness produces that vulnerability — workers unable to complain about wages or conditions — while verification duties and sanction exposure impose compliance costs. Gain and cost arrive through the same statute.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, employers_of_deportable_labor, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, employers_of_deportable_labor, payer).

% Treaty monitors, courts, and agencies that press non-refoulement, detention-condition, and family-life standards against state practice. Holds no vote in the doctrine it critiques; its instruments bind only where states accept them, and its findings are answered as external opinion rather than constitutive limit.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_bodies, excluded,
    institutional, generational, trapped, global).

% Analyzes the doctrinal question of what sovereignty entails, documents the historical contingency of passport and admission regimes, and publishes the counterexamples — pre-WWI open travel, microstates, free-movement unions — that test the constitutive claim. Influence runs through argument and citation, not enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, territorial_states).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the membership-boundary problem every territorial political community faces: deciding who belongs, administering entry and residence, and maintaining the demographic, security, and welfare perimeter within which citizenship obligations and entitlements are tracked. Stated without evaluation: whatever else it does, the arrangement coordinates the allocation of membership.
% TRANSFER_FUNCTION: Moves legal status and mobility: forecloses entry and residence to non-citizens at state discretion, transferring life-plans, labor, and safety-seeking away from would-be entrants and toward the bounded community. Secondarily moves money — enforcement appropriations from taxpayers to vendors, visa and fine revenue to treasuries — and moves risk onto migrants, who absorb interception, detention, and dangerous-route mortality.
% ABSENT_VOICES: The governed outsiders: would-be migrants and asylum seekers are subject to decisions made entirely in forums they cannot enter — the classic objection that those coerced should have a voice is registered nowhere inside the frame. Origin states bearing remittance dependence and skill drain also lack a seat; human rights bodies speak only from outside, which this reading counts as external opinion, not standing.
% DISAPPEARANCE_RATIONALE: If admission discretion and its enforcement vanished overnight, movement would resume along corridors now policed shut, wage and demographic structures in destination states would shift within seasons, the enforcement procurement complex would lose its purchaser, and citizenship itself would lose the boundary that defines it — the political geography of membership would reorganize around whatever successor arrangement emerged.
% FOUNDING_PROBLEM: Consolidating administrative control over population movement: war-making states needed to tax, conscript, and track their populations, and the WWI emergency standardized the passport and visa machinery that earlier dynasties had used intermittently. Admission discretion was built as an instrument of war finance and internal order, later repurposed for labor management and identity maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by migration historiography — notably the scholarship tracing the passport to a wartime administrative monopoly over the legitimate means of movement — and by UNHCR and IOM reporting on how today's machinery descends from those instruments. The stronger claim that the discretion is constitutive of statehood as such is attested mainly by the doctrine's own proponents and allied jurisprudence; no source outside the benefiting parties vouches for that constitutive version, which is itself signal.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).
:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim/metric independence: the claimed type is tangled_rope because the structure shows both a genuine coordination function (membership allocation and boundary administration that any successor arrangement would have to reproduce) and asymmetric imposition (migrant seats bear severe costs the beneficiary seats do not), actively enforced — all three canonical requirements are met by declaration, not tuning. The sovereignty_primary reading itself presents the arrangement as constitutive and therefore mountain-shaped; that self-presentation is recorded in the axioms and the constitutive-status omega, not reconciled with the metrics. Epsilon is reading-indexed over the fixed referent (the standing arrangement): from this reading's own lights, exclusion costs are the price of a constitutive function rather than rent, so epsilon is authored low (0.32) relative to what the sibling readings would author over the identical arrangement — the residual excess (procurement capture, closure-created smuggling rents, externalized mortality) is what keeps it above zero even here. Suppression (0.80) is a raw structural property, unscaled by power or scope: the machinery — patrols, carrier sanctions, detention capacity, pushback — applies force regardless of anyone's judgment of its legitimacy; it is overwhelmingly structural (physical, legal, financial barriers) with a thin internalized layer (deterred claiming) riding on it. Theater (0.38) is below the proxy-replacement threshold but rising steeply after 2001 and 2015: symbolic barrier construction and headline removal statistics serve domestic audiences alongside real interdiction. Accessibility collapse (0.60): lawful channels close but irregular substitution persists, so alternatives degrade rather than vanish — typical of an enforced hybrid rather than a natural limit. Resistance (0.65): collective caravans, sanctuary jurisdictions, strategic litigation, search-and-rescue fleets, and origin-state diplomacy; the payer seats' principal lever is coalition, converting individual powerlessness into intermittent organized pressure, and the regime's case-by-case processing and dispersal practices fragment exactly that coalition — fragmentation is part of the enforcement function. Election cycles oscillate enforcement intensity around the rising trend; the shared eight-point grid samples the trend, not the cycle. All three series run on one shared time grid, every metric authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state seat the arrangement is self-defense of statehood — the thing being defended is the defender's own constitution. From the migrant seats the same structure is total foreclosure of movement with lethal margins and no procedural voice. From the citizen seat it is a membership good with diffuse fiscal and reputational costs. From the vendor seat it is a revenue line. Citizen seats additionally carry an identity-lock character: national membership is partly constitutive of self-concept, so open-entry positions register as self-abolition rather than policy preference; if that frame broke, the citizen seat's assessment would migrate toward the jurisdictional reading. Inter-institutionally, states and human-rights bodies hold nominally comparable institutional power but radically different standing: the bodies can name violations but cannot cast votes inside the doctrine, which is precisely the external-limit treatment this reading assigns them. Same-level lateral differentiation: employers and the enforcement industry are both well-resourced beneficiaries with mobile exit, but the employer's gain arrives as a byproduct of strictness it publicly opposes, while the vendor's gain arrives by direct appropriation it actively campaigns to enlarge.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial_states sit at the beneficiary end (agenda_setter plus declared beneficiary, arbitrage exit): the arrangement subsidizes them with control, revenue, and political credit. Citizen_majorities derive low directionality from their beneficiary declaration, tempered above the floor by taxation, sectoral labor shortage, and reputational cost — a genuine but impure subsidy. Border_enforcement_industry and employers_of_deportable_labor are beneficiaries with mobile exit, sitting nearest the arbitrage end; the employer's benefit is indirect (produced by strictness), the vendor's direct (procurement). Excluded_would_be_migrants and asylum_seekers sit at the target end, amplified by trapped exit: no lawful channel, no standing, no alternative but dangerous substitution. Human_rights_bodies (excluded) and international_law_scholars (observer) sit outside the beneficiary/victim derivation — commentary-grade seats, not correction-grade. Receipt: the arrangement's gains demonstrably accrue to the state seat — fee and fine revenue, procurement it directs, and the political capital of asserted control — so gain_flow names territorial_states; vendor receipts are derivative of state purchasing choices. Fixing cost is prohibitive: dissolving the arrangement would require renegotiating a constitutive self-understanding held across the entire state system simultaneously, and the seat best placed to lead that change is the seat collecting from the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — war-era administrative control of population movement — is contested rather than dead: the security and registration functions persist, but whether today's exclusion machinery still serves them or persists as identity assertion is disputed, so the mismatch consumer reads status=contested x verdict=world_rearranges and no dead-mandate zombie flag fires automatically; the open question routes to the omegas instead. The classification blocks two opposite mislabels. Against the pure-snare mislabel: the membership-coordination function is genuine and demanded by any successor arrangement, so the transfer cannot be read as cover alone. Against the piton mislabel: a concentrated beneficiary exists (the state seat collects directly), the administrator could change the rules at will, and the cost-asymmetry test for inertia fails — the party able to fix it is the party paid by it. The conservative identity_coordination floor does the remaining work: the reading's own 'belonging has a price' framing is exactly the cover story the floor refuses to launder, keeping the asymmetric migrant-side costs visible as more than coordination overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the sovereignty_primary reading of kernel border_control_legitimacy; what would the sibling readings change structurally if instantiated?',
    'Compile and classify all three reading files side by side; compare authored epsilon, victim sets, computed per-seat types, and drift states across the shared referent.',
    'freedom_of_movement_primary would convert excluded migrants from payers of a legitimate constitutive price into holders of a violated fundamental right and author epsilon far higher; jurisdictional_sovereignty would narrow the victim set to exclusions lacking balancing justification and attribute less of the enforcement apparatus to sovereignty defense. Cross-reading deltas locate the disagreement in the entailment premise, not in the facts of enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading indexicality of the border_control_legitimacy kernel.').

omega_variable(
    constitutive_status_of_border_control,
    'Is admission discretion genuinely constitutive of statehood — a conceptual necessity no state could lack — or a historically contingent arrangement that states benefit from presenting as constitutive?',
    'Test the constitutive claim against counterexamples: pre-WWI mass travel without universal passports, microstates and dependencies with minimal admission machinery, free-movement unions that preserve member statehood internally; combine with conceptual analysis of statehood criteria (Montevideo) that nowhere require border closure.',
    'If contingent, the reading''s constitutive self-presentation fails and the arrangement stands as a constructed hybrid of coordination and imposed costs; if genuinely constitutive, part of what sibling readings count as violation is misdescribed and the mountain-shaped self-presentation regains footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_status_of_border_control, conceptual, 'Natural-law versus constructed ambiguity of the constitutive-border-control thesis.').

omega_variable(
    absolute_discretion_vs_qualified_practice,
    'Is the discretion exercised in practice absolute, or already qualified by non-refoulement, human-rights treaty bodies, and mobility agreements — such that the absolute-discretion premise describes an idealized regime rather than the standing one?',
    'Comparative analysis of treaty compliance, regional and international court rulings, and externalization agreements: measure how often exclusion decisions are overridden by qualifying norms and whether states accept the overrides as binding.',
    'If practice is heavily qualified, this reading instantiates a constraint drifting away from the arrangement it claims to describe — the drift vector bends toward jurisdictional_sovereignty and the victim set shrinks to cases where qualifiers fail; if discretion remains operative despite the qualifications, the reading tracks practice and the qualifiers are exactly the external limits this reading already counts them as.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_discretion_vs_qualified_practice, empirical, 'Gap between the absolute-discretion premise and qualified state practice.').

omega_variable(
    enforcement_capture_share,
    'How much of the arrangement''s cost side reflects private capture — detention and surveillance procurement, smuggling rents created by closure — rather than the minimum cost of the sovereignty-defense function as this reading understands it?',
    'Cost accounting of enforcement contracts against independent security-outcome estimates; natural experiments where enforcement scaled up or down (corridor closures, regularization episodes) tracking vendor revenue and smuggling-price response.',
    'Isolates the excess component even within this reading''s own lights: a large capture share means the authored low epsilon understates what the reading''s own principles should count as beyond-defense cost, pushing the effective classification toward the sibling readings'' assessments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capture_share, empirical, 'Private-capture share of enforcement cost versus defense-function cost.').

omega_variable(
    externalization_scope_attribution,
    'Do externalized controls — pushbacks, offshore processing, third-country transfer deals — count as exercises of the state''s own admission discretion (inside this constraint) or as separable arrangements of transit states?',
    'Legal analysis of extraterritorial jurisdiction doctrines and funding flows: who finances, directs, and bears legal responsibility for interdiction beyond the border line.',
    'Sets the victim-set boundary and the effective spatial scope: attributing externalization to the discretionary power extends the constraint''s reach continent-wide and deepens migrant-seat directionality; treating it as separable shrinks both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externalization_scope_attribution, conceptual, 'Scope attribution of externalized enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1914, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t1914, border_control_legitimacy__sovereignty_primary, theater_ratio, 1914, 0.08).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t1914, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.12).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t1945, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t1965, border_control_legitimacy__sovereignty_primary, theater_ratio, 1965, 0.16).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t1965, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t1985, border_control_legitimacy__sovereignty_primary, theater_ratio, 1985, 0.2).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t1985, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t1995, border_control_legitimacy__sovereignty_primary, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t1995, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t2001, border_control_legitimacy__sovereignty_primary, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t2001, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t2015, border_control_legitimacy__sovereignty_primary, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t2015, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t2024, border_control_legitimacy__sovereignty_primary, theater_ratio, 2024, 0.38).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bcl_sovereignty_primary_be_t1914, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1914, 0.14).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t1914, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t1945, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t1965, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1965, 0.21).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t1965, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t1985, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1985, 0.24).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t1985, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t1995, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1995, 0.27).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t1995, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t2001, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2001, 0.29).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t2001, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t2015, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2015, 0.31).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t2015, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t2024, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2024, 0.32).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bcl_sovereignty_primary_su_t1914, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1914, 0.25).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t1914, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t1945, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t1965, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t1965, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t1985, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t1985, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t1995, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t1995, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t2001, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2001, 0.71).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t2001, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t2015, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t2015, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t2024, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2024, 0.8).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'border control legitimacy' conflates three structurally distinct claims (epsilon-invariance decomposition): (1) sovereignty_primary — discretion absolute and constitutive (this file, epsilon authored low from its own lights); (2) freedom_of_movement_primary — a fundamental right of movement that the same arrangement violates (high epsilon); (3) jurisdictional_sovereignty — regulatory authority with contingent closure authority (mid epsilon, narrower victim set). The readings share one referent — the standing global border-control arrangement — and diverge only in assessment; each is a separate file with its own beneficiaries, victims, and type. Upstream/downstream: the sovereignty-primary doctrine is the traditional baseline against which the other two readings define themselves, so this file's edges point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
