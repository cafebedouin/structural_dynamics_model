% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Foundational State Authority to Exclude Non-Members (Sovereignty-Primary Reading)
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the contested
 *   border_normative_status kernel: territorial boundaries are legitimate
 *   instruments of collective self-determination, and states hold
 *   foundational — not merely defeasible or licensed — authority to exclude
 *   non-members. The standing arrangement under contest is the actual global
 *   border regime operated under that warrant: universal visa systems,
 *   physical enforcement at land and sea frontiers, detention and removal
 *   machinery, externalized processing in transit states, and fee-financed
 *   admission queues. Per the reading's declared structural delta, excluded
 *   migrants enter the victim set, border enforcement is treated as a
 *   legitimate state function, and the harms displaced onto migrants and
 *   transit communities are handled as externalities rather than costs of the
 *   arrangement itself. Under this reading the core exclusion function is
 *   classified as legitimate boundary maintenance rather than extraction; the
 *   reading concedes as defective a residual band of practices — detention
 *   profiteering, exploitation of undocumented workers whose deportability
 *   suppresses their wage bargaining, fee economies, and enforcement risk
 *   pushed onto crossing routes. The authored metrics describe that
 *   concession honestly; the claimed type (tangled_rope) asserts this
 *   reading's own structural verdict: a genuine coordination function
 *   (constituting the demos, pooling welfare risk, matching voters to the
 *   governed) operating through the same machinery that imposes asymmetric,
 *   actively enforced costs on people with no voice in it. The claim and the
 *   metrics are independent authored facts; the engine computes per-seat
 *   classifications from the structural data. Sibling readings of the same
 *   kernel — freedom_primary and qualified_sovereignty — are separate
 *   constraints linked in the network; they author different epsilon over the
 *   identical referent. KEY AGENTS (by structural relationship): -
 *   destination_state_governments: Agenda-setter (institutional/constrained)
 *   — administers exclusion, collects fees and enforcement legitimacy -
 *   citizens_of_destination_states: Primary beneficiary (organized/mobile) —
 *   receives the prospect, labor-market, and welfare-pool surplus -
 *   excluded_aspiring_migrants: Primary target (powerless/trapped) — bears
 *   denial of entry and foreclosed life prospects - rejected_asylum_seekers:
 *   Target (powerless/trapped) — protection claims closed by the same
 *   authority - undocumented_resident_workers: Target inside the line
 *   (powerless/trapped) — status precarity converts presence into
 *   exploitability - mixed_status_families: Collateral target
 *   (moderate/trapped) - border_enforcement_contractors: Incidental
 *   beneficiary (organized/arbitrage) — collects enforcement budgets -
 *   transit_state_communities: Externalized-cost bearer
 *   (moderate/constrained) - migrant_rights_organizations: Advocacy observer
 *   (organized/analytical) - international_human_rights_bodies: Institutional
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.44).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.74).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.44).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Foundational State Authority to Exclude Non-Members (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'b47e08b1-1ec8-40fb-84ee-adb1aada9d74').
narrative_ontology:cs_kernel_codification('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', distributed).
narrative_ontology:cs_authority_grounding('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', lineage).
narrative_ontology:cs_interpretation_layer_present('b47e08b1-1ec8-40fb-84ee-adb1aada9d74').
narrative_ontology:cs_reading_relation('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', foundational, collective_self_determination_grounds_exclusion).
narrative_ontology:cs_axiom_status(collective_self_determination_grounds_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', collective_self_determination_grounds_exclusion, deontological).
narrative_ontology:cs_axiom('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', secondary, membership_priority_over_arrival_claims).
narrative_ontology:cs_axiom_status(membership_priority_over_arrival_claims, holdable).
narrative_ontology:cs_axiom_grounding('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', membership_priority_over_arrival_claims, conventional).
narrative_ontology:cs_reference_frame('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', foundational_territorial_exclusion_authority).
narrative_ontology:cs_drift_state('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', contemporary_human_rights_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('b47e08b1-1ec8-40fb-84ee-adb1aada9d74', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizens_of_destination_states).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, destination_state_governments).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, border_enforcement_contractors).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_aspiring_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, undocumented_resident_workers).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, mixed_status_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, transit_state_communities).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, westphalian_sovereignty_principle).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, bounded_demos_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate admission criteria, operate visa and asylum systems, command border and removal agencies, and collect application-fee revenue. Bear enforcement costs, court review, and diplomatic friction. Can alter policy only within electoral mandates, treaty commitments, and domestic courts; unilateral relaxation of exclusion is politically ruinous, unilateral intensification is cheap.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, destination_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, destination_state_governments, beneficiary).

% Elect the governments that set admission rules and receive the membership surplus: labor-market access, welfare-pool eligibility, residential security, and the prospect differential over non-members. Fund enforcement through taxation. Can emigrate if dissatisfied, at personal cost, and retain membership privileges abroad in many cases.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizens_of_destination_states, beneficiary,
    organized, biographical, mobile, national).

% Seek entry for work, family reunification, or safety and encounter visa denials, decade-long queues, or physical barriers. Their alternatives are indefinite immobility, irregular crossing with mortal risk, or paying intermediaries. They hold no vote in any jurisdiction whose rules decide their case; their exit from the constraint is the very entry being denied.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_aspiring_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution and present protection claims, facing admissibility screens, safe-third-country routing, and externalized processing that pushes assessment away from destination territory. Return or redirection exposes them to the danger they fled; the same exclusion authority that admits discretionary migrants closes their last exit.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Live and work inside destination states without status. Deportability converts their presence into bargaining weakness: sub-standard wages, withheld complaints about abuse, no access to labor courts. Leaving forfeits livelihoods and community ties built over years; staying compounds precarity. Employers capture the discount their status creates.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Households split by status: a deportable parent, a citizen child, a spouse awaiting a visa that never arrives. Enforcement forces choices between family unity and residence; relocation abroad separates breadwinners from children's schooling and healthcare. Every member bears costs of a rule none of them set.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, mixed_status_families, payer,
    moderate, biographical, trapped, national).

% Supply surveillance technology, detection systems, detention beds, transport, and processing staff under government contract. Revenues scale with enforcement intensity and detention volume. If border demand fell, they could redirect sales to other security markets; their exposure to the constraint is one-sided.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, border_enforcement_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Host stranded movers, outsourced detention, rescue burdens, and smuggling economies along major routes. Receive development and security aid conditioned on enforcement cooperation. Cannot refuse the externalized functions without losing assistance; the destination states' enforcement choices become their local facts.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, transit_state_communities, payer,
    moderate, biographical, constrained, regional).

% Litigate, document abuses, and advocate for the excluded; gather testimony the agenda-setting governments do not collect. Hold no admission authority and depend on donations independent of the arrangement. Their seat is analytical and adversarial: they can contest the constraint's operation but not administer it.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, migrant_rights_organizations, observer,
    organized, generational, analytical, global).

% Monitor compliance with refugee and human-rights treaties, publish findings, censure states, and occasionally litigate. Depend on state cooperation for access and enforcement. Cannot themselves admit anyone; their leverage is reputational and doctrinal rather than operational.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, citizens_of_destination_states).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constitutes and maintains bounded political communities: fixes who belongs to the demos, keeps the electorate coextensive with the population subject to the state's laws and taxes, pools social-insurance risk within a defined membership, and gives the community collective control over the size and composition of its population.
% TRANSFER_FUNCTION: Moves life-prospect, labor-market, and residential-security surplus from excluded non-members to members of destination states; moves application-fee revenue and enforcement budgets from applicants and taxpayers into state coffers and contractor balance sheets; moves the physical risks of movement onto the movers themselves (route mortality, detention, family separation).
% ABSENT_VOICES: The excluded themselves: aspiring migrants and rejected asylum seekers have no vote in any jurisdiction whose admission rules decide their case; their objections reach the process only vicariously, through advocacy organizations, origin-state diplomacy, and treaty bodies — seats that can testify but cannot admit. Transit-state communities absorb externalized enforcement without having agreed to host it.
% DISAPPEARANCE_RATIONALE: If foundational exclusion authority vanished overnight, the membership architecture of the interstate system would rearrange: welfare states would face unbounded claimant pools and rebuild membership rules in some form; labor markets would redistribute across former frontiers; citizenship would lose its gating function and be renegotiated; destination-state politics, built substantially on admission control, would reorganize around whatever successor distinction between member and non-member emerged.
% FOUNDING_PROBLEM: Constituting stable political communities with defined membership after the collapse of empire and dynastic order: determining who owes allegiance, who may vote, who may claim the community's mutual aid, and how a self-governing people can govern itself when the governed set is undefined.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the destination-beneficiary set: migrant-origin states police their own borders and maintain nationality laws, attesting that the membership-constitution problem is general rather than a destination-state rent; post-colonial states invoked the same self-determination doctrine to found their own memberships; international-relations scholarship independently documents the Westphalian genealogy. What is NOT corroborated from outside is the sufficiency of this reading's solution: human-rights treaty bodies and the sibling readings attest the founding problem is live while disputing that foundational exclusion is its required answer.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).
:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44 from this reading's own lights: the core exclusion transfer (denied entry, foreclosed prospects) is classified by the reading as legitimate boundary maintenance, not extraction, so it does not load epsilon; what loads epsilon is the conceded excess — detention and surveillance contracting that profits from enforcement volume, employer capture of the wage bargain against deportable workers, visa-fee revenue decoupled from processing cost, and enforcement risk displaced onto crossing routes. Suppression is authored at 0.74 as a raw, unscaled structural property: the arrangement is held up by physical infrastructure — barriers, patrols, detention beds, removal flights — and by legal devices (carrier sanctions, safe-third-country rules) that close alternative routes; the reading regards this coercion as authorized, but authorization does not reduce its descriptive magnitude, and unlike interpersonal cases the suppression here is almost entirely structural rather than internalized. Theater_ratio 0.48: a growing share of enforcement activity is staged for domestic electorates (symbolic barrier construction, deportation flights timed to news cycles, deterrence announcements contradicted by admission realities) rather than performing migration management. Accessibility_collapse 0.45: alternatives remain live — free-movement unions exist and expand, humanitarian-corridor and open-admission proposals circulate, and the EU's internal border abolition demonstrates the arrangement is not a natural constant. Resistance 0.58: route adaptation, sanctuary networks, rescue flotillas, strategic litigation, and diaspora politics meet the enforcement apparatus continuously. The temporal series run on one shared eight-point grid (1914-2026) so every metric is authored at every examined time point; trajectories are monotonic ratchets rather than cycles — election-driven oscillations are short-lived against the century-scale enforcement buildup, so no intermittent-reinforcement dynamic is claimed. Receipt surface: the arrangement's gains demonstrably accrue to the citizen seat (prospect differential, welfare-pool integrity, labor-market insulation), with governments collecting fee revenue and contractors collecting enforcement budgets as secondary streams; fixing the constraint for whoever could fix it — coordinated redesign of the membership architecture — is prohibitive relative to any single seat's benefit, which is why no government attempts it unilaterally.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the agenda-setter seat (destination_state_governments) the arrangement is the machinery of a legitimate sovereign function it administers; from the beneficiary seat (citizens) it is the guarantee that the demos they vote in matches the population subject to the state's laws; from the payer seats it is a gate operated entirely by others over the terms of their own lives — excluded_aspiring_migrants and rejected_asylum_seekers hold no vote in any jurisdiction deciding their case, and undocumented_resident_workers experience the constraint as the convertibility of their person into a discounted wage. The sibling readings institutionalize this gap: freedom_primary effectively adopts the payer seat's verdict as the verdict, which is why the same referent carries a different epsilon there. Coalition capacity among the powerless victims is weak: the excluded are jurisdictionally scattered and legally disenfranchised, transit states hold latent coalition leverage but are aid-conditioned into enforcement cooperation, and the organized advocacy seats speak for the excluded without being able to admit anyone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the beneficiary end: citizens_of_destination_states (mobile exit, organized power) sit nearest it — the arrangement subsidizes their prospect differential; destination_state_governments combine agenda-setting with fee revenue and enforcement legitimacy, keeping them near the beneficiary end despite bearing enforcement costs; border_enforcement_contractors collect enforcement budgets with arbitrage-grade exit, damping their effective-extraction exposure toward subsidy. Victim declarations drive d toward the target end: excluded_aspiring_migrants and rejected_asylum_seekers are trapped (their exit from the constraint IS the entry being denied), pushing them toward the full-target end; undocumented_resident_workers are trapped inside the line, where deportability is the extraction mechanism; mixed_status_families are trapped by household-level stakes; transit_state_communities are constrained by aid conditionality. Scope amplification applies at the global scale of the arrangement — verifying humane treatment across every frontier is harder than within any one jurisdiction, which scales effective extraction upward modestly for the target seats. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure extraction (the freedom_primary temptation) would erase the genuine coordination function — demos constitution, welfare-pool integrity, voter/governed correspondence — that even hostile analysis must account for; reading it as pure coordination (the sovereignty triumphalist temptation) would launder the asymmetric, voice-less imposition of costs that the victim declarations record. Mandatrophy status: the founding problem — constituting bounded political communities capable of self-government and mutual aid — is live, not dead: states still confront membership definition, and the arrangement's persistence tracks that live problem rather than inertial habit. Accordingly the consistent pairing holds: founding_problem_status=live with disappearance_verdict=world_rearranges, so no capture/zombie flag is expected. The theater trajectory (0.12 to 0.48) is nonetheless the watch item: if performative enforcement continues substituting for functional management, the piton pathway opens even under a live mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the sovereignty_primary reading of the border_normative_status kernel; how would instantiating the freedom_primary or qualified_sovereignty readings instead change the structural data?',
    'Authoring the sibling stories: freedom_primary moves every excluded mover into the victim set as a rights-bearer wronged and strips the enforcement function of legitimate-coordination credit; qualified_sovereignty retains the victim set but converts unconditional exclusion into conditioned exercise, adding proportionality and human-rights obligations as structural requirements.',
    'Under freedom_primary the same standing arrangement loses its coordination-function credit and computes toward snare; under qualified_sovereignty it remains tangled_rope with mandated mitigation duties. This file''s epsilon (0.44) is indexed to the sovereignty reading''s assessment; the sibling files author different epsilon over the identical referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this is one of three readings of the border_normative_status kernel; sibling readings rewrite the victim set and the justification burden.').

omega_variable(
    exclusion_naturalness_vs_construction,
    'Is bounded-membership exclusion a near-universal feature of durable political organization (approaching a structural constant) or a constructed institution maintained because identifiable insiders benefit?',
    'Comparative-historical study of borderless or open-membership polities (city-league citizenship, imperial subjecthood, free-movement unions) and natural experiments where internal borders dissolved (Schengen enlargement): do coordination benefits persist without exclusion, and who loses?',
    'If exclusion approaches naturality, the measured epsilon shifts toward irreducible coordination cost and the rope component strengthens; if constructed, beneficiary-weighted extraction rises and the snare component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_naturalness_vs_construction, empirical, 'Whether exclusion authority is a structural feature of political organization or a constructed, beneficiary-serving institution.').

omega_variable(
    identity_frame_extraction_cover,
    'Does the collective-identity rationale for exclusion track genuine boundary-maintenance needs, or does it operate as cover for insider economic interests (labor-market protection, welfare-pool conservation) that would survive refutation of the identity argument?',
    'Decompose admission-policy variation: compare policy restrictiveness where identity framing and insider economic exposure diverge; test whether the identity rationale predicts policy in cases with no underlying insider economic interest.',
    'If cover, the coordination function downgrades toward enforcement-only and effective extraction rises sharply; the identity_coordination Boltzmann floor would no longer shelter the coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_frame_extraction_cover, empirical, 'Whether identity framing launders economic rent-seeking at the border.').

omega_variable(
    externalized_cost_perimeter,
    'Does the sovereignty reading''s epsilon understate extraction because the arrangement''s gravest costs (crossing mortality, transit-state detention burdens, origin-country skill depletion) fall outside the perimeter the reading counts as its business?',
    'Full-cost accounting extending the ledger to migrant mortality rates, transit-state fiscal and social burdens, and origin-country human-capital losses; recompute epsilon under the extended perimeter.',
    'A materially higher extended-perimeter epsilon would push this reading''s own assessment toward the qualified_sovereignty neighborhood and suggest the legitimacy claim depends on perimeter choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_cost_perimeter, conceptual, 'Accounting-perimeter dependence of the reading''s extraction assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1914, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1914, border_normative_status__sovereignty_primary, theater_ratio, 1914, 0.12).
narrative_ontology:measurement_basis(bord_tr_t1914, observed).
narrative_ontology:measurement(bord_tr_t1938, border_normative_status__sovereignty_primary, theater_ratio, 1938, 0.2).
narrative_ontology:measurement_basis(bord_tr_t1938, observed).
narrative_ontology:measurement(bord_tr_t1952, border_normative_status__sovereignty_primary, theater_ratio, 1952, 0.15).
narrative_ontology:measurement_basis(bord_tr_t1952, observed).
narrative_ontology:measurement(bord_tr_t1974, border_normative_status__sovereignty_primary, theater_ratio, 1974, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1974, observed).
narrative_ontology:measurement(bord_tr_t1993, border_normative_status__sovereignty_primary, theater_ratio, 1993, 0.3).
narrative_ontology:measurement_basis(bord_tr_t1993, observed).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__sovereignty_primary, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(bord_tr_t2005, observed).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__sovereignty_primary, theater_ratio, 2015, 0.44).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2026, border_normative_status__sovereignty_primary, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(bord_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1914, border_normative_status__sovereignty_primary, base_extractiveness, 1914, 0.18).
narrative_ontology:measurement_basis(bord_be_t1914, observed).
narrative_ontology:measurement(bord_be_t1938, border_normative_status__sovereignty_primary, base_extractiveness, 1938, 0.3).
narrative_ontology:measurement_basis(bord_be_t1938, observed).
narrative_ontology:measurement(bord_be_t1952, border_normative_status__sovereignty_primary, base_extractiveness, 1952, 0.26).
narrative_ontology:measurement_basis(bord_be_t1952, observed).
narrative_ontology:measurement(bord_be_t1974, border_normative_status__sovereignty_primary, base_extractiveness, 1974, 0.33).
narrative_ontology:measurement_basis(bord_be_t1974, observed).
narrative_ontology:measurement(bord_be_t1993, border_normative_status__sovereignty_primary, base_extractiveness, 1993, 0.36).
narrative_ontology:measurement_basis(bord_be_t1993, observed).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__sovereignty_primary, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement_basis(bord_be_t2005, observed).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__sovereignty_primary, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2026, border_normative_status__sovereignty_primary, base_extractiveness, 2026, 0.44).
narrative_ontology:measurement_basis(bord_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1914, border_normative_status__sovereignty_primary, suppression_requirement, 1914, 0.2).
narrative_ontology:measurement_basis(bord_su_t1914, observed).
narrative_ontology:measurement(bord_su_t1938, border_normative_status__sovereignty_primary, suppression_requirement, 1938, 0.35).
narrative_ontology:measurement_basis(bord_su_t1938, observed).
narrative_ontology:measurement(bord_su_t1952, border_normative_status__sovereignty_primary, suppression_requirement, 1952, 0.33).
narrative_ontology:measurement_basis(bord_su_t1952, observed).
narrative_ontology:measurement(bord_su_t1974, border_normative_status__sovereignty_primary, suppression_requirement, 1974, 0.42).
narrative_ontology:measurement_basis(bord_su_t1974, observed).
narrative_ontology:measurement(bord_su_t1993, border_normative_status__sovereignty_primary, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement_basis(bord_su_t1993, observed).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__sovereignty_primary, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(bord_su_t2005, observed).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__sovereignty_primary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2026, border_normative_status__sovereignty_primary, suppression_requirement, 2026, 0.74).
narrative_ontology:measurement_basis(bord_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border control' conflates three structurally distinct normative claims about the same standing arrangement. This file (sovereignty_primary) authors epsilon 0.44 for the arrangement as the sovereignty reading assesses it — core exclusion classified as legitimate boundary maintenance, residual epsilon carried by conceded excesses. The sibling freedom_primary file authors a substantially higher epsilon over the identical referent (all exclusion loads as impermissible restriction); the qualified_sovereignty file authors an intermediate epsilon with proportionality obligations as structural requirements. Upstream/downstream: the sovereignty reading is the historically entrenched baseline whose dominance shapes the operating environment of the other two; the freedom reading is cited as the normative challenge the other two must answer. Each file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
