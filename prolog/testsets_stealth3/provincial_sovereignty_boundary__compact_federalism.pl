% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism Reading of the Provincial Sovereignty Boundary
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested provincial-sovereignty
 *   kernel: the compact-federalism account, in which Confederation was a
 *   bargain struck among sovereign provinces that retained residual
 *   sovereignty, federal authority is therefore conditional on continuing
 *   provincial consent, equalization is a negotiated transfer rather than an
 *   entrenched entitlement, national climate policy is subject to provincial
 *   override, and exit from the union is a matter of negotiation rather than
 *   permission. The epsilon referent is this standing compact arrangement
 *   itself, assessed as it actually operates — never the rival arrangements
 *   the sibling readings would install. The claim and the metrics are
 *   independent authored facts: claimed_type states tangled_rope because the
 *   structure demonstrably carries both a genuine pooling function and an
 *   asymmetric extraction function through the same consent machinery, while
 *   the metrics describe the arrangement's actual operation without being
 *   tuned to any predicted verdict.
 *
 * KEY AGENTS:
 *   - - resource_producing_provincial_governments: primary beneficiary (institutional/arbitrage) — collect resource revenues and veto concessions; commodities route around federal reach via global markets
 *   - - federal_government_of_canada: principal payer (institutional/trapped) — its discretionary authority is the quantity the consent-conditionality taxes; it also co-administers the machinery
 *   - - recipient_province_treasuries: secondary beneficiary (institutional/constrained) — dependent on renegotiable transfers
 *   - - net_contributor_province_taxpayers: payer (organized/constrained) — fund equalization; lever is voice, not exit
 *   - - interprovincial_mobile_workers: payer (moderate/constrained) — nominal mobility, uniform barrier regime follows them
 *   - - indigenous_nations: payer + excluded (organized/trapped) — bound by settlements they never signed; pressing for nation-to-nation standing
 *   - - linguistic_minority_communities: payer (moderate/identity_locked) — depend on protections above the provincial layer; exit would dissolve the community
 *   - - supreme_court_of_canada: analytical observer (institutional/analytical) — adjudicates the kernel contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.48).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Reading of the Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '7c28dda1-cf0a-4591-9cd3-e800b42f57a0').
narrative_ontology:cs_kernel_codification('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', fixed_text).
narrative_ontology:cs_authority_grounding('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', lineage).
narrative_ontology:cs_interpretation_layer_present('7c28dda1-cf0a-4591-9cd3-e800b42f57a0').
narrative_ontology:cs_reading_relation('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', foundational, provinces_retained_residual_sovereignty).
narrative_ontology:cs_axiom_status(provinces_retained_residual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', provinces_retained_residual_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', foundational, secession_negotiable_under_clear_majority).
narrative_ontology:cs_axiom_status(secession_negotiable_under_clear_majority, holdable).
narrative_ontology:cs_axiom_grounding('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', secession_negotiable_under_clear_majority, conventional).
narrative_ontology:cs_reference_frame('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', founders_compact_among_sovereign_provinces).
narrative_ontology:cs_drift_state('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', post_secession_reference_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7c28dda1-cf0a-4591-9cd3-e800b42f57a0', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_producing_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, recipient_province_treasuries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government_of_canada).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, net_contributor_province_taxpayers).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, interprovincial_mobile_workers).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, linguistic_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern energy-producing provinces. Collect royalties and resource revenues under provincial ownership of natural resources, shielded from federal pricing instruments by the consent-veto this arrangement grants them. Leverage the credible threat of withholding consent — pipeline approvals, emissions caps, equalization terms — to renegotiate federal initiatives downward. Their commodities sell on global markets regardless of federal climate frameworks, so revenue continues even when national policy is refused; withdrawing from the federation altogether remains a political card rather than a plan.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_producing_provincial_governments, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_producing_provincial_governments, agenda_setter).

% Administers the national government: defense, currency, treaties, and nationwide programs. Every extension of activity into provincial spheres — climate pricing, healthcare conditions, resource corridors — must be purchased with provincial consent, negotiated side-agreements, or tolerated litigation. Collects contributions from richer provinces and redistributes them through equalization formulas the provinces repeatedly reopen. It cannot leave the arrangement it administers; its discretionary authority is the quantity this arrangement conditions.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government_of_canada, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government_of_canada, agenda_setter).

% Run provinces whose own-source revenues fall short of service obligations and receive equalization transfers to close the gap. The transfers arrive under formulas subject to periodic renegotiation and side-deal carve-outs, so budget planning carries recurring reopening risk. Declining the transfers outright would force deep service cuts or steep local taxation; the dependence is real, but the terms are never fully theirs to set.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, recipient_province_treasuries, beneficiary,
    institutional, biographical, constrained, regional).

% Households and firms in provinces paying more into federal coffers than they receive back, part of which funds equalization. Mobilize periodically — referenda on removing clauses, fair-deal commissions, separatist polling surges — to demand renegotiation. Relocating to another province changes their exposure little, since contribution status follows economic geography; their practical instrument is collective political voice, not exit.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, net_contributor_province_taxpayers, payer,
    organized, biographical, constrained, regional).

% Work and do business across provincial lines and pay the price of provincially controlled credentials, professional licensing, transport rules, and internal-trade exceptions at every crossing. Formally free to move anywhere in the country; in practice the barrier regime follows them, since every province maintains its own walls. They bear the efficiency losses of a fragmented internal market without any seat at the table that maintains it.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, interprovincial_mobile_workers, payer,
    moderate, immediate, constrained, national).

% Hold treaty relationships with the Crown and unresolved title claims across lands that provincial governments administer and provincial resource regimes develop. Were never parties to the provincial bargain, yet its land-use and resource settlements bind their territories. Pursue recognition case by case through title litigation, negotiate impact-benefit agreements project by project, and press for direct nation-to-nation standing that the provincial layer of the arrangement continuously displaces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded).

% Francophone communities outside Quebec and anglophone communities inside it depend on protections anchored above the provincial layer. When provincial governments homogenize language and schooling policy, these communities bear the costs directly, and their recourse runs through federal intervention that the consent-veto makes conditional. Leaving the province would mean dissolving the community itself, so relocation is not a usable remedy; survival depends on the durability of higher-level commitments.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, linguistic_minority_communities, payer,
    moderate, generational, identity_locked, regional).

% Adjudicates disputes between the orders of government. Its rulings have repeatedly located sovereignty in the imperial and federal constitutional frame rather than in a compact of pre-existing provinces, while simultaneously affirming that a clear majority on a clear question obliges the federation to negotiate. Each reference decision redraws the boundary the arrangement operates along, and the courtroom is where competing accounts of the founding are tested against each other.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court_of_canada, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, resource_producing_provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools continental-scale functions — defense, customs, currency, transcontinental infrastructure — across formerly separate colonies while preserving each province's control over its own society-defining jurisdictions; the consent requirement is the device that lets distinct polities share one economy without merging their governments.
% TRANSFER_FUNCTION: Moves equalization contributions from net-contributor provinces to recipient provinces under renegotiable formulas; moves policy authority upward to the federal order only as far as provinces consent to lend it; moves veto and override leverage downward to provincial capitals; and moves the costs of fragmented standards onto anyone living or trading across provincial lines.
% ABSENT_VOICES: Indigenous nations were never signatories to the provincial bargain yet live under its land and resource settlements (seated here as excluded-payer). Municipal governments, created and abolished by provinces, have no seat at any table that allocates their powers. Future generations inherit the climate consequences of provincial override without present representation.
% DISAPPEARANCE_RATIONALE: If the consent-conditionality and negotiable-exit architecture vanished overnight, the federation would reorganize around whichever account replaced it: a subordination frame would recentralize taxing and environmental authority and harden exit rules; a resource-primacy frame would fragment fiscal union. Provincial budgets built on equalization, federal programs built on conditional spending, and cross-country credential and trade regimes would all need rebuilding — the arrangement is load-bearing, not ornamental.
% FOUNDING_PROBLEM: Uniting separate North American colonies for common defense and continental economics without forcing their distinct societies — language, civil law, religion, and later resource endowments — under a single homogeneous government.
% FOUNDING_PROBLEM_CORROBORATION: Judicial references and constitutional scholarship written outside the benefiting governments attest both halves: the protective problem was real and remains argued (minority-language litigation and western resource grievances still generate cases and commissions), while the compact-genesis version of the solution is expressly rejected by the courts that nonetheless endorsed the negotiation duty — attesting the problem's liveness without certifying this reading's genealogy. No attestation comes from the arrangement's own beneficiaries alone.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the consent-veto converts national initiatives into bilaterally negotiated tolls; negotiable equalization holds redistribution hostage to donor leverage; provincial override shifts diffuse national and global climate costs onto concentrated provincial revenue gains. It stops well short of maximum because the pooling layer — currency, defense, customs, a continental market — delivers real, retained benefits to every seat, and because the exit channel remains open-ended rather than closed. Suppression 0.48: persistence requires continuous active enforcement (litigation, fiscal brinkmanship, clarity-era gatekeeping on referenda), but below the range where exits and alternatives are shut — negotiated exit and rival readings remain legally discussable. Theater ratio 0.31: first-ministers summitry grows more ceremonial, but substantive bargaining still occurs behind the ritual, so performative maintenance is a symptom, not the core. Accessibility collapse 0.38: rival architectures (recentralization, asymmetric federalism, resource absolutism, dissolution) stay publicly viable once the arrangement is understood — understanding does not dissolve the alternatives. Resistance 0.60: sustained federal pushback, consistent judicial rejection of the compact genealogy, minority litigation, and inter-provincial counter-mobilization. The temporal series run on one shared grid (T=0..40, mapped to 1982..2022) with all three metrics authored at every point; the stepwise rise in extractiveness tracks discrete constitutional episodes (Meech collapse, Secession Reference, Kyoto ratification fight, equalization formula wars, carbon-pricing confrontation, emissions-cap standoff) rather than a smooth drift, and the suppression series ratchets up through the clarity-law era and then plateaus — enforcement capacity hardened once and stabilized.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should: from the federal seat the same arrangement is a cage that prices every extension of national authority; from the resource-producer seat it is a shield that guarantees revenue and negotiating leverage; recipient treasuries experience a lifeline whose terms are perpetually reopened over their heads; net-contributor households experience a levy they can contest only collectively; mobile workers experience fragmentation as friction at every border crossing; Indigenous and linguistic-minority seats experience the identical structure as the displacement of their protection onto a layer that can be vetoed by the very governments they need protection from. The engine computes this divergence from the authored structural data — the claimed type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive directionality near the subsidized end: resource producers combine beneficiary role with arbitrage-grade exit (global commodity markets bypass federal frameworks), placing them nearest zero; recipient treasuries are beneficiaries but with constrained exit and renegotiation exposure, so slightly less subsidized. Payers derive directionality toward the target end, modulated by exit: trapped and identity_locked payers (federal government, Indigenous nations, linguistic minorities) sit farther toward full-target than mobile-in-name-only workers and voice-dependent contributor households. The federal seat is the one case where naive derivation could overshoot: as payer-and-trapped it derives near full-target, but it is simultaneously half the administrative apparatus the constraint runs through and a co-setter of the agenda, so its true position is conditioned-administrator rather than pure target — this is recorded qualitatively here rather than as an override, because role differentiation (payer with secondary agenda_setter) plus trapped exit already encodes the asymmetry without flattening the other institutional seats onto the same corrected value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinct societies sharing a continent — still has live defenders and live costs, so the mandate has not outlived its function and no mandatrophy resolution is declared. The tangled_rope classification is what prevents the two standard mislabels: reading the arrangement as pure coordination flatters federalism and hides the veto-toll extraction riding on the consent machinery; reading it as pure extraction flatters centralization and erases the real pooling benefits every seat retains. Holding both components in one classification preserves the diagnostic question the corpus exists to ask: how much of the observed friction is the irreducible price of plural federation, and how much is rent collected through the consent gate? If equalization hardened into an entrenched entitlement and the override lever were withdrawn, the extraction component would thin toward coordination; if Indigenous exclusion hardened into uncompensated appropriation, the consent cover would fail and the arrangement would slide toward the family's extraction boundary — both transitions are tracked by the omegas and the temporal series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_genesis_empirical_record,
    'Does the historical-imperial record show the colonies entering union as sovereign polities contracting among themselves, or as subordinate possessions federated by imperial statute?',
    'Archival analysis of the London Resolutions, drafting correspondence behind the British North America Act, and imperial parliamentary proceedings, read against the subsequent line of judicial characterizations.',
    'If the compact genesis fails empirically, this reading''s foundational legitimacy collapses toward the constitutional_subordination sibling; if it holds, the consent-conditionality and negotiable-exit clauses gain independent warrant and the measured extraction reads as priced consent rather than imposed structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compact_genesis_empirical_record, empirical, 'Empirical status of the compact-genesis claim anchoring this reading.').

omega_variable(
    sibling_delta_authority_relocation,
    'This constraint is one reading (compact_federalism) of the provincial_sovereignty_boundary kernel; what changes structurally if the constitutional_subordination sibling reading governs instead?',
    'Compare the compiled sibling story: the sovereignty source relocates from provincial consent to the imperial-federal frame; the federal seat flips from payer toward agenda-setter; the provincial veto becomes delegated and revocable discretion; the exit clause hardens from negotiation to permission.',
    'Under the sibling, the same constitutional surface computes with inverted beneficiary/victim sets and materially different extraction — the readings are separate constraints, not one constraint measured two ways, which is why they are authored as separate files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_authority_relocation, conceptual, 'Committer-frame delta versus the constitutional_subordination sibling reading.').

omega_variable(
    sibling_delta_absolute_resource_claim,
    'What changes structurally if the resource_sovereignty_primacy sibling reading governs instead?',
    'Compare the compiled sibling story: provincial ownership hardens into absolute territorial sovereignty, the negotiation clause disappears, exit becomes moot or unilateral, and federal and Indigenous seats become pure targets of provincial control.',
    'Extraction concentrates on the federal and Indigenous seats while the coordination component thins toward cover — the sibling sits nearer this constraint family''s pure-extraction boundary, and the compact reading''s influence edge marks the pathway by which its leverage logic arms that harder claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_absolute_resource_claim, conceptual, 'Committer-frame delta versus the resource_sovereignty_primacy sibling reading.').

omega_variable(
    equalization_entitlement_status,
    'Is equalization a constitutionally committed obligation or a politically negotiable transfer?',
    'Judicial treatment of the constitutional commitment to reasonably comparable services at reasonably comparable taxation, together with the stability record of formula rewrites and side-deal carve-outs across the interval.',
    'An entrenched entitlement would cut away the negotiability lever, lowering effective extraction on recipient treasuries and shifting the arrangement toward pure coordination; confirmed negotiability sustains the donor-leverage extraction documented in this story''s metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_entitlement_status, empirical, 'Security of the fiscal-transfer pillar under this reading.').

omega_variable(
    indigenous_consent_trajectory,
    'Can the provincial layer of this arrangement persist if Indigenous title claims harden into recognized co-sovereignty over the lands provinces administer?',
    'Track title jurisprudence, treaty-implementation settlements, and direct nation-to-nation fiscal arrangements over coming decades.',
    'Hardening title would remove the consent foundation beneath provincial resource administration, pushing this reading''s operation from negotiated extraction toward uncompensated appropriation — a slide toward the family''s extraction boundary with attendant reclassification pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_trajectory, empirical, 'Durability of the consent foundation beneath provincial resource administration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psb_compact_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.22).
narrative_ontology:measurement(psb_compact_tr_t8, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 8, 0.26).
narrative_ontology:measurement(psb_compact_tr_t12, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 12, 0.27).
narrative_ontology:measurement(psb_compact_tr_t16, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 16, 0.28).
narrative_ontology:measurement(psb_compact_tr_t20, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 20, 0.28).
narrative_ontology:measurement(psb_compact_tr_t26, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 26, 0.29).
narrative_ontology:measurement(psb_compact_tr_t32, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 32, 0.29).
narrative_ontology:measurement(psb_compact_tr_t36, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 36, 0.3).
narrative_ontology:measurement(psb_compact_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(psb_compact_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(psb_compact_be_t8, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(psb_compact_be_t12, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(psb_compact_be_t16, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(psb_compact_be_t20, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(psb_compact_be_t26, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 26, 0.56).
narrative_ontology:measurement(psb_compact_be_t32, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(psb_compact_be_t36, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(psb_compact_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(psb_compact_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(psb_compact_su_t8, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(psb_compact_su_t12, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(psb_compact_su_t16, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(psb_compact_su_t20, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(psb_compact_su_t26, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 26, 0.47).
narrative_ontology:measurement(psb_compact_su_t32, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(psb_compact_su_t36, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 36, 0.48).
narrative_ontology:measurement(psb_compact_su_t40, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'provincial rights / provincial sovereignty' covers three structurally distinct arrangements with different extraction profiles, beneficiary/victim sets, and exit mechanics, so it is authored as three stories over the shared kernel provincial_sovereignty_boundary. This story instantiates the compact_federalism reading. The compact reading is upstream of the resource_sovereignty_primacy reading (its consent-leverage logic supplies the rhetorical and legal foundation the absolutist resource claim radicalizes) and coexists with the constitutional_subordination reading (the judicially entrenched frame both rivals contest). Each file carries its own epsilon, its own stakeholders, and its own claimed type; the files are linked through affects_constraints rather than averaged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
