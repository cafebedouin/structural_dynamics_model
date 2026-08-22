% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Ordnung Technology Separation — Principle Reading (Structural Non-Entanglement)
 *   domain: religious/technological/commitment_systems
 *
 * SUMMARY:
 *   This file instantiates the principle reading of the contested
 *   Gelassenheit separation kernel: separation means avoiding structural
 *   entanglement in worldly systems (grid utilities, commercial insurance,
 *   debt, networked media), and a technology is acceptable exactly when its
 *   use can be functionally isolated from those systems. Solar panels,
 *   pneumatic tools, and diesel-hydraulic equipment are permitted because
 *   they do work without contracts, accounts, or data flows; grid
 *   electricity, commercial insurance, and internet are forbidden regardless
 *   of whether an isolated use could be imagined, because the systems
 *   themselves are the entanglement. The constraint is the Ordnung's
 *   technology-governance regime read through this criterion — one of three
 *   sibling readings of the same kernel. The artifact reading (forbid what
 *   resembles worldly artifacts regardless of function) and the consequence
 *   reading (evaluate technology by its effect on visiting, mutual aid, and
 *   rootedness) are separate constraint stories with their own epsilon values
 *   and victim sets; this file's epsilon is authored only for the principle
 *   reading's arrangement, by its own lights, and is lower than the artifact
 *   reading's because the function-based criterion removes the arbitrary
 *   prohibitions that generate theatrical enforcement. The claim/metric gap
 *   is deliberate per corpus rules: claimed_type is authored from structural
 *   belief (tangled_rope — genuine coordination with real asymmetric costs),
 *   the metrics from descriptive belief, independently; where the engine's
 *   per-seat computation diverges from the claim, that divergence is the
 *   measurement.
 *
 * KEY AGENTS:
 *   - district_ministry_bishops_and_ministers: agenda-setter (institutional/identity_locked) — administers the Ordnung, chosen by lot, gains continuity not rent
 *   - church_community_members: primary beneficiary with payer costs (moderate/identity_locked) — receives mutual aid and autonomy, forgoes worldly systems
 *   - entanglement_desiring_members: primary target (moderate/identity_locked) — bears recurring costs of refused internet, insurance, and grid access
 *   - shunned_former_members: severe target (powerless/trapped) — under the ban, social death inside the natal world
 *   - amish_youth_pre_baptism: cost-bearing excluded seat (moderate/constrained) — bears the constraint's claim on their future without a seat in its setting
 *   - off_grid_technology_vendors: secondary beneficiary (moderate/mobile) — the function-isolation rule defines their market
 *   - worldly_institution_providers: excluded external seat (institutional/mobile) — refused customers, no say in the refusal
 *   - anabaptist_scholarship: analytical observer — distinguishes the readings; accounts feed back into public justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.44).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.55).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Ordnung Technology Separation — Principle Reading (Structural Non-Entanglement)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technological/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '8e09984e-73f5-4d8c-8a18-a2393c3284a9').
narrative_ontology:cs_kernel_codification('8e09984e-73f5-4d8c-8a18-a2393c3284a9', distributed).
narrative_ontology:cs_authority_grounding('8e09984e-73f5-4d8c-8a18-a2393c3284a9', practice).
narrative_ontology:cs_interpretation_layer_present('8e09984e-73f5-4d8c-8a18-a2393c3284a9').
narrative_ontology:cs_reading_relation('8e09984e-73f5-4d8c-8a18-a2393c3284a9', gelassenheit_separation__artifact_reading, influences).
narrative_ontology:cs_reading_relation('8e09984e-73f5-4d8c-8a18-a2393c3284a9', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('8e09984e-73f5-4d8c-8a18-a2393c3284a9', foundational, separation_means_structural_non_entanglement).
narrative_ontology:cs_axiom_status(separation_means_structural_non_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('8e09984e-73f5-4d8c-8a18-a2393c3284a9', separation_means_structural_non_entanglement, theological).
narrative_ontology:cs_axiom('8e09984e-73f5-4d8c-8a18-a2393c3284a9', secondary, functional_isolation_suffices_for_technology).
narrative_ontology:cs_axiom_status(functional_isolation_suffices_for_technology, holdable).
narrative_ontology:cs_axiom_grounding('8e09984e-73f5-4d8c-8a18-a2393c3284a9', functional_isolation_suffices_for_technology, instrumental).
narrative_ontology:cs_reference_frame('8e09984e-73f5-4d8c-8a18-a2393c3284a9', gelassenheit_structural_independence).
narrative_ontology:cs_drift_state('8e09984e-73f5-4d8c-8a18-a2393c3284a9', contemporary_business_technology_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8e09984e-73f5-4d8c-8a18-a2393c3284a9', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, church_community_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, off_grid_technology_vendors).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, entanglement_desiring_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, shunned_former_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, amish_youth_pre_baptism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, church_community_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, gelassenheit_nonconformity_doctrine).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, congregational_mutual_aid_sufficiency).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, ordinung_congregational_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chosen by lot from the baptized membership to preach, administer discipline, and convene the twice-yearly Ordnung affirmation. They rule on how the separation principle applies to each new technology — in the current period, cell phones, solar inverters, and business internet lines. They gain the continuity of the institution they steward, but the office carries no salary and no property beyond their neighbors'; exit would mean the loss of office, standing, and the community that constitutes their entire life.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, district_ministry_bishops_and_ministers, agenda_setter,
    institutional, generational, identity_locked, local).

% Baptized members who farm and run small shops under the Ordnung. They receive the mutual aid network that pays their medical and disaster losses, the care structures that look after their elderly without state institutions, and an identity that gives their work and worship coherence. They pay by forgoing grid power, commercial insurance, and home internet, and by submitting technology questions to district judgment; leaving would mean shunning by their own parents and children.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, church_community_members, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, church_community_members, payer).

% Members — often shop owners and young families — whose trades or circumstances pull toward worldly systems: a metal shop that wants internet for orders, a family facing a catastrophic diagnosis who wonders about commercial coverage, a farmer eyeing a grid-tied solar subsidy. They comply, work around the rule through intermediaries and off-grid substitutes, or leave and lose everything social. Their costs are concrete and recurring; their voice in Ordnung deliberation is informal.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, entanglement_desiring_members, payer,
    moderate, biographical, identity_locked, local).

% Baptized members who adopted forbidden technology or otherwise broke with the Ordnung and did not return to confess. The district and their own families decline table, commerce, and ordinary speech with them — social death inside the only world most have ever known. Re-entry is possible through public confession; life outside means starting over among the English with skills and habits built for a different economy.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, shunned_former_members, payer,
    powerless, biographical, trapped, regional).

% Young people in the years before baptism, when Ordnung discipline does not yet bind them. They sample the world's technologies and freedoms, then face the choice: baptism means lifelong submission to the Ordnung, refusal means losing the community, family closeness, and the future the community offers. They have no seat in the Ordnung deliberations that define what they would be submitting to; most return, some do not.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, amish_youth_pre_baptism, excluded,
    moderate, biographical, constrained, local).

% Solar installers, pneumatic-tool suppliers, and diesel-engine adapters who serve Amish districts. The function-isolation rule defines their market: equipment that does the work without grid contracts, utility accounts, or networked services sells; anything requiring entanglement does not. They profit from the boundary and adapt their offerings to its rulings; their exit is easy and their stake is commercial only.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, off_grid_technology_vendors, beneficiary,
    moderate, biographical, mobile, regional).

% Utilities, insurers, telecom carriers, and state programs that would sell or extend services to Amish households and businesses. The Ordnung forbids members from accepting what they offer, so potential customers decline by rule; these providers have no seat in the deliberations that refuse them and no recourse beyond ordinary marketing at the boundary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, worldly_institution_providers, excluded,
    institutional, generational, mobile, continental).

% Scholars of Anabaptist life who document and theorize how the communities govern technology. They distinguish the readings of separation — appearance, practice-effects, structural entanglement — and their accounts feed back into how the communities and courts articulate what the Ordnung is for.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, anabaptist_scholarship, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the boundary-maintenance collective-action problem: any member's unilateral adoption of worldly systems (grid power, commercial insurance, internet) would entangle the whole community — shared infrastructure precedents, adverse selection against the mutual aid fund, and erosion of the congregational authority that adjudicates disputes. The Ordnung coordinates refusal so that structural independence and internal risk-pooling remain intact for all.
% TRANSFER_FUNCTION: Moves risk-pooling and care obligations from commercial insurers and state programs into the congregational mutual aid network; moves members' communication, commerce, and attention away from worldly platforms and infrastructure toward local, community-mediated channels; moves adjudicative authority over technology questions to the district ministry. What members give up is access to worldly systems; what the community pools in exchange is collective independence.
% ABSENT_VOICES: Pre-baptism youth have no formal seat in Ordnung deliberation though they bear its claim on their futures; entanglement-desiring members voice objections privately, but the twice-yearly corporate affirmation pressures dissent into silence; members under the ban are structurally absent — their testimony about costs cannot be heard inside the district; the worldly institutions themselves are absent by the community's choice, never by their own consent.
% DISAPPEARANCE_RATIONALE: If the separation norm vanished overnight, entanglement would proceed household by household within a decade: grid connections and commercial insurance would undercut the mutual aid fund by adverse selection, internet commerce would reroute attention and loyalty outward, and the congregational authority that depends on adjudicating the boundary would lose its object. The mutual aid system that replaces insurance, and the community as a distinct self-governing polity, would not survive the rearrangement — the districts would dissolve into their surroundings within a generation or two.
% FOUNDING_PROBLEM: The Anabaptist founding problem: how can the church remain a voluntary, disciplined community distinct from the worldly order — state power, market status, oath and violence — while embedded within it (Gelassenheit, yieldedness to God and community over self)? The technology-specific form arose with industrialization: how to take useful techniques without letting worldly infrastructure — utilities, insurance, debt, mass media — restructure communal life and authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: Anabaptist scholarship (Hostetler, Kraybill and successors) attests separation as constitutive and ongoing; the community's litigated public defenses — the Social Security exemption of 1965 and Wisconsin v. Yoder (1972) — required articulating the founding problem to external courts and Congress; former-member memoirs attest both the reality of the separation problem and its costs. No source outside the beneficiary set attests that the problem is dead.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Interval mapping: t=0 approximates 1920, t=100 approximates 2020 — a century of Ordnung evolution. Extractiveness 0.44, referent: the standing Ordnung technology regime as the principle reading assesses it — not any endorsed alternative. By this reading's own lights the arrangement is largely justified: the costs members bear (forgone grid power, insurance, internet) are the price of the structural independence the reading holds central, and the mutual aid network compensates much of what insurance would. The residual extraction is real: entanglement-desiring members bear recurring uncompensated costs, the shunned bear catastrophic ones, and the internet ban may be over-inclusive relative to the reading's own criterion (see omegas). Suppression 0.55 is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation; the enforcement machinery (district discipline, confession, the ban) does perhaps 40% of the holding work while internalized Gelassenheit formation does roughly 60%, a split carried by the suppression_mechanism omega. Theater 0.22: the Ordnung functionally coordinates boundary and mutual aid; visible markers serve boundary function under this reading rather than standing as the criterion, so performative maintenance is limited. Accessibility collapse 0.45: worldly alternatives remain visible and partially accessible — Rumspringa sampling, daily work contact with English systems — so the constraint forbids rather than forecloses, far from a natural law's ~0.85. Resistance 0.4: youth pushback, business lobbying for phones and internet, district-to-district variation, and occasional affiliation splits absorb pressure without organized opposition. Identity-lock dynamics: baptism constitutes the self — exit means shunning by parents and children plus, as members understand it, jeopardy to salvation; if that identity frame broke through mass defection, the constraint's suppression would reveal itself as structural and the payer seats' effective extraction would rise. Same-level dynamics: contented members and entanglement-desiring members hold identical formal standing (baptized membership, identity_locked exit) but different exposure — business owners and families facing medical catastrophe meet the constraint as a recurring cost, the retired and uncommercial meet it as background order. Inter-institutional dynamics: the district ministry versus state institutions — the Social Security refusal and Yoder litigation forced the community to articulate the principle reading to external courts, and those episodes built the enforcement machinery visible in the suppression series' mid-century rise. The measurement series runs on one shared nine-point grid so every metric is authored at every examined time point; the century-scale trajectory is drift, not cycle — the semi-annual Ordnung affirmation is a micro-cycle absorbed by the interpretive layer rather than an oscillation in these metrics.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from one structure. From the ministry seat the Ordnung is covenant order — the coordination it stewards and the continuity it gains make the arrangement read as rope-like. From the entanglement-desiring member's seat the same structure is a binding on commerce, communication, and risk — extraction with identity-locked exit. From the shunned member's seat it is social death — near-full target. From the vendor's seat it is a market definition — pure benefit. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. Across readings the gap sharpens: under the artifact reading the same household's solar panels would be a violation (appearance), under the consequence reading a cell phone might be permitted (it aids mutual aid) — each seat's experience of the constraint depends on which criterion its district actually applies. Coalition note: the shunned are individually powerless, but coalition power is structurally blocked — shunning isolates each exmember from the others' knowledge networks and from the district's deliberation, so the usual powerless-coalition remedy does not assemble here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: church_community_members hold the pooled goods the refusal makes possible (mutual aid viability, autonomy, identity) — low d, damped by their identity-locked position inside the benefit. off_grid_technology_vendors collect a defined market — low d with mobile exit, the nearest-to-arbitrage beneficiary seat. The ministry administers and accrues continuity and adjudicative authority — a real but non-material, lot-bounded gain; it is not a transfer of the extracted costs. The receipt check behind gain_flow='diffuse' examined every named seat affirmatively: the ministry accrues an authority byproduct, not rents; the member collective receives its own pooled goods (self-benefit, not capture); vendors are paid for services rendered, not for the constraint's extraction; members' forgone access is foreclosed option-value, not a transferred good — no seat captures the extraction. Targets: entanglement_desiring_members (payer, identity_locked — trapped amplification toward full-target), shunned_former_members (powerless, trapped — nearest full-target), amish_youth_pre_baptism (excluded payer with constrained exit — moderate-high d; their retention leverage keeps them off the extreme), worldly_institution_providers (mild target — refused customers, no say). District-local scope keeps verification feasible, so the engine's scope amplification on extraction is modest. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already separate the ministry's benefit-side d from the worldly providers' mild target-side d despite their shared institutional power atom. FNL gaming check on the identity_coordination declaration: the identity framing is genuine here — the Ordnung coordinates membership boundary against evolving technology criteria — but coupling that concentrated extraction on powerless agents at large scope would be nonsensical regardless of the identity offset; here the concentrated extraction (the shunned) sits at district-local scope and the coupling is moderate, not the Power-by-Scope pathology.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — remaining a distinct, self-governing community while embedded in worldly systems — is live: every new entanglement vector (broadband, cell service, grid-tie inverters, state programs) re-poses it, and the founding_problem_status by disappearance_verdict pair (live by world_rearranges) is consistent, so no zombie flag is expected. The tangled_rope claim prevents mislabeling in both directions: the coordination function (boundary maintenance, mutual-aid viability, congregational adjudication) is genuine, so the real victims do not make this a snare; the asymmetric costs (shunning, forgone risk-pooling for the medically fragile, the internet ban's possible over-inclusiveness) prevent rope. If the founding problem ever died — worldly systems dissolving, or the community assimilating — the Ordnung would decay toward piton: theatrical maintenance of separation markers without the structure they once guarded. The theater_ratio series (0.10 rising to 0.22) is watched for exactly that signature, and its current low level is evidence the function still runs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the gelassenheit_separation kernel — the principle reading (separation as structural non-entanglement). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative analysis across districts and affiliations whose stated criterion differs: appearance-based (artifact), practice-effects-based (consequence), and structural-entanglement-based (principle) districts can be compared on permitted-technology sets, victim sets, and theater ratios.',
    'The artifact reading would forbid functionally isolated but worldly-looking technology — higher theater and a different victim set (users of innocuous modern-looking tools). The consequence reading would permit entangling technology that aids visiting and mutual aid while forbidding isolated technology that erodes practice — shifting both epsilon and the victim set. The disagreement is located in the criterion of separation itself, not in separation''s importance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is the principle reading of the gelassenheit_separation kernel; sibling readings would change the criterion, victim set, and epsilon.').

omega_variable(
    internet_isolation_separability,
    'The principle reading forbids internet use regardless of isolation possibility. Is internet use necessarily structurally entangling (data flows, commerce, attention capture), or are uses separable that would satisfy the reading''s own criterion?',
    'District-level natural experiments where limited business internet was permitted (order-entry terminals, intermediary-hosted storefronts): track whether entanglement followed — networked-service creep, attention reallocation, dependency formation — or the use stayed functionally isolated.',
    'If separable uses exist and stay isolated, the ban is over-inclusive relative to the reading''s own principle and the measured extraction includes unjustified refusal; if entanglement reliably follows, the ban is a principled application and the authored epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_isolation_separability, empirical, 'Whether the categorical internet ban tracks the reading''s own criterion or over-reaches it.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (shunning, family severance, district discipline) or internalized (Gelassenheit formation, guilt, identity fusion)?',
    'Post-exit suppression trajectory of former members: if fear and guilt persist and shape behavior years after exit, a substantial share is internalized; if exmembers shed it quickly, the structural machinery carries the weight.',
    'If largely internalized, the structural suppression measure understates the constraint''s hold — the constraint travels with the member after exit, and per-seat suppression for identity-locked members is higher than authored; if largely structural, enforcement decay would release members quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a community governed by identity fusion and discipline.').

omega_variable(
    mutual_aid_substitution_adequacy,
    'Does the congregational mutual aid network actually substitute for commercial insurance at catastrophic scale — long-term care, congenital condition, lifelong disability — or do the weakest members absorb uncompensated risk?',
    'Mutual aid fund claims data and catastrophic-case outcomes across settlements, compared against the actuarial cost of equivalent commercial coverage.',
    'If substitution is inadequate, the constraint''s extraction (forgoing insurance) falls disproportionately on medically fragile members, widening the victim set and raising epsilon above what the reading''s own justification covers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_aid_substitution_adequacy, empirical, 'Whether the internal risk-pooling that justifies the insurance ban holds at the catastrophic tail.').

omega_variable(
    emic_criterion_underdetermination,
    'Is ''structural non-entanglement'' the community''s own operative criterion (emic) or an analyst''s rationalization (etic), with the lived criterion closer to the consequence reading''s practice-effects test?',
    'Ordnung deliberation records and minister interviews: do districts justify technology rulings in structural-independence terms or in practice-preservation terms (visiting, mutual aid, rootedness)?',
    'If the lived criterion is consequence-based, this file''s epsilon and victim set misdescribe the operative constraint — the consequence reading would be the better instantiation, and the principle reading exists mainly as public-justification language for courts and outsiders. The declared kernel framing here (distributed codification, practice authority) would then be describing the justification layer rather than the operating layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emic_criterion_underdetermination, conceptual, 'CS-framing under-determination: whether the declared criterion represents the community''s actual framing or an external rationalization of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gela_tr_t0, observed).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__principle_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(gela_tr_t10, observed).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__principle_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(gela_tr_t20, observed).
narrative_ontology:measurement(gela_tr_t35, gelassenheit_separation__principle_reading, theater_ratio, 35, 0.15).
narrative_ontology:measurement_basis(gela_tr_t35, observed).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__principle_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(gela_tr_t50, observed).
narrative_ontology:measurement(gela_tr_t65, gelassenheit_separation__principle_reading, theater_ratio, 65, 0.18).
narrative_ontology:measurement_basis(gela_tr_t65, observed).
narrative_ontology:measurement(gela_tr_t80, gelassenheit_separation__principle_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(gela_tr_t80, observed).
narrative_ontology:measurement(gela_tr_t90, gelassenheit_separation__principle_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement_basis(gela_tr_t90, observed).
narrative_ontology:measurement(gela_tr_t100, gelassenheit_separation__principle_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(gela_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(gela_be_t0, observed).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__principle_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(gela_be_t10, observed).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__principle_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(gela_be_t20, observed).
narrative_ontology:measurement(gela_be_t35, gelassenheit_separation__principle_reading, base_extractiveness, 35, 0.46).
narrative_ontology:measurement_basis(gela_be_t35, observed).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__principle_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement_basis(gela_be_t50, observed).
narrative_ontology:measurement(gela_be_t65, gelassenheit_separation__principle_reading, base_extractiveness, 65, 0.43).
narrative_ontology:measurement_basis(gela_be_t65, observed).
narrative_ontology:measurement(gela_be_t80, gelassenheit_separation__principle_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement_basis(gela_be_t80, observed).
narrative_ontology:measurement(gela_be_t90, gelassenheit_separation__principle_reading, base_extractiveness, 90, 0.46).
narrative_ontology:measurement_basis(gela_be_t90, observed).
narrative_ontology:measurement(gela_be_t100, gelassenheit_separation__principle_reading, base_extractiveness, 100, 0.44).
narrative_ontology:measurement_basis(gela_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(gela_su_t0, observed).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__principle_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(gela_su_t10, observed).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__principle_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(gela_su_t20, observed).
narrative_ontology:measurement(gela_su_t35, gelassenheit_separation__principle_reading, suppression_requirement, 35, 0.56).
narrative_ontology:measurement_basis(gela_su_t35, observed).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__principle_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(gela_su_t50, observed).
narrative_ontology:measurement(gela_su_t65, gelassenheit_separation__principle_reading, suppression_requirement, 65, 0.54).
narrative_ontology:measurement_basis(gela_su_t65, observed).
narrative_ontology:measurement(gela_su_t80, gelassenheit_separation__principle_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(gela_su_t80, observed).
narrative_ontology:measurement(gela_su_t90, gelassenheit_separation__principle_reading, suppression_requirement, 90, 0.57).
narrative_ontology:measurement_basis(gela_su_t90, observed).
narrative_ontology:measurement(gela_su_t100, gelassenheit_separation__principle_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(gela_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Amish technology rules' decomposes into three readings of one kernel, per the epsilon-invariance principle — one kernel (gelassenheit_separation), three constraints. The artifact reading (appearance criterion) carries higher theater and a victim set of users of innocuous modern-looking tools; the consequence reading (practice-effects criterion) permits entangling-but-community-serving tools and forbids isolated-but-practice-eroding ones; this principle reading (structural-entanglement criterion) permits off-grid function and forbids internet and insurance categorically. Epsilon differs across the family because the criterion determines which costs count as justified; this file authors epsilon only for the principle reading's arrangement. Upstream, the shared Gelassenheit theology grounds all three; downstream, this reading's articulation — forced by litigation (the 1965 Social Security exemption, Wisconsin v. Yoder 1972) — has become the public-justification frame and exerts structural pressure on the artifact reading's legitimacy (hence the influences edge to it), while coexisting with the consequence reading as rival operative criteria across districts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
