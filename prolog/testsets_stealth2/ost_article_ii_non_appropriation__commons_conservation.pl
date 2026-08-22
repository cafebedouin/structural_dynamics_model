% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation - Commons Conservation Reading
 *   domain: international law/space governance/commons
 *
 * SUMMARY:
 *   This story instantiates the commons_conservation reading of the Outer
 *   Space Treaty's Article II kernel: the claim that 'use or occupation ...
 *   by any other means' reaches de facto appropriation through resource
 *   extraction, and that the resulting prohibition binds states and - through
 *   Article VI supervision - private actors alike. On this reading, no state
 *   or company may take celestial resources absent multilateral
 *   authorization; first-mover extraction investments stand on no ownable
 *   title; the non-spacefaring majority retains a veto over enclosure; and
 *   any distribution of benefits happens by negotiation rather than arrival
 *   order. The reading is one of three live interpretations of the same text:
 *   an extraction-permissive sibling treats recovered resources as private
 *   property compatible with the treaty, and an international-regime sibling
 *   defers the whole question to a future framework. The claim/metric gap is
 *   deliberate and load-bearing: the reading is CLAIMED as tangled
 *   coordination - genuine commons preservation welded to asymmetric costs -
 *   while the authored metrics describe its actual sixty-year operation:
 *   weakly enforced, increasingly contested, with a rising share of
 *   declaratory maintenance. The engine computes per-seat types from the
 *   structural data; divergence between the claim and the computed verdicts
 *   is the datum. Epsilon's referent is the standing arrangement under
 *   contest - the prohibition as it actually operates, declaratory baseline
 *   plus custodial interpretation - assessed by this reading's own lights,
 *   which price the imposed costs as the legitimate price of preservation
 *   rather than as rent; the endorsed alternative (a fully operative
 *   multilateral regime) is not the referent. KEY AGENTS (by structural
 *   relationship): - nonspacefaring_states: Primary beneficiary bloc
 *   (organized/constrained) - preserves collective veto over enclosure; gains
 *   standing, not transferred value - small_spacefaring_states: Secondary
 *   beneficiary (moderate/constrained) - gains capability-neutral rules -
 *   future_generations: Deferred beneficiary (powerless/trapped) - holds
 *   preserved option value, no seat of its own - private_mining_ventures:
 *   Primary target (powerful/arbitrage) - bears stranded-investment costs,
 *   exits by re-domiciling - spacefaring_state_commercial_sectors: Secondary
 *   target (powerful/arbitrage) - bears compliance uncertainty and foregone
 *   market upside - copuos_legal_subcommittee: Agenda setter
 *   (institutional/identity_locked) - custodial administrator, collects
 *   authority-rents - artemis_accord_signatories: Excluded dissenters
 *   (institutional/mobile) - built a parallel normative track outside the
 *   consensus room - rival_program_major_powers: Dual-positioned hedge
 *   (institutional/mobile) - defends the commons against rivals while
 *   planning its own resource use - space_law_scholarship: Analytical
 *   observer (analytical/analytical) - supplies the arguments all camps
 *   consume
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.55).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.44).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.55).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation - Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international law/space governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, 'b020136f-d41d-4994-ae14-b75e4b9356d3').
narrative_ontology:cs_kernel_codification('b020136f-d41d-4994-ae14-b75e4b9356d3', fixed_text).
narrative_ontology:cs_authority_grounding('b020136f-d41d-4994-ae14-b75e4b9356d3', lineage).
narrative_ontology:cs_interpretation_layer_present('b020136f-d41d-4994-ae14-b75e4b9356d3').
narrative_ontology:cs_reading_relation('b020136f-d41d-4994-ae14-b75e4b9356d3', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('b020136f-d41d-4994-ae14-b75e4b9356d3', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('b020136f-d41d-4994-ae14-b75e4b9356d3', foundational, extraction_constitutes_appropriation).
narrative_ontology:cs_axiom_status(extraction_constitutes_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('b020136f-d41d-4994-ae14-b75e4b9356d3', extraction_constitutes_appropriation, deontological).
narrative_ontology:cs_axiom('b020136f-d41d-4994-ae14-b75e4b9356d3', secondary, collective_authorization_precondition).
narrative_ontology:cs_axiom_status(collective_authorization_precondition, holdable).
narrative_ontology:cs_axiom_grounding('b020136f-d41d-4994-ae14-b75e4b9356d3', collective_authorization_precondition, conventional).
narrative_ontology:cs_reference_frame('b020136f-d41d-4994-ae14-b75e4b9356d3', commons_preservation_baseline).
narrative_ontology:cs_drift_state('b020136f-d41d-4994-ae14-b75e4b9356d3', contemporary_artemis_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b020136f-d41d-4994-ae14-b75e4b9356d3', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, nonspacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, small_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, private_mining_ventures).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_state_commercial_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, rival_program_major_powers).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, rival_program_major_powers).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, common_heritage_of_mankind_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__commons_conservation, res_communis_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly four-fifths of UN member states with no independent launch or extraterrestrial operations capability. They ratified the Outer Space Treaty as their principal protection against being permanently shut out of celestial bodies by whichever powers arrive first. Their leverage is collective: voting weight and agenda influence in the UN Committee on the Peaceful Uses of Outer Space, and the moral-legal standing of the 1967 bargain. Leaving the treaty system would strip them of the only framework in which they hold any standing, so their practical course is to defend the treaty's interpretive integrity inside it. What they secure is a preserved veto over unilateral enclosure and a seat at any future allocation negotiation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, nonspacefaring_states, beneficiary,
    organized, generational, constrained, global).

% States with emerging launch, satellite, or research programs that lack the capital for planetary resource operations. Several hedge diplomatically, joining both the multilateral process and the rival bilateral accords, but structurally they gain from rules that bind larger powers equally, since no capability of theirs could resist a great-power enclosure after the fact. Their exposure is limited: little domestic industry bears compliance costs, and their scientific-access interests are served by authorization systems rather than harmed by them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, small_spacefaring_states, beneficiary,
    moderate, biographical, constrained, global).

% The population that will inherit whatever allocation settles over celestial resources. They hold preserved option value - the possibility that resources remain unenclosed until governance matures - and bear none of the current compliance costs. They cannot consent, object, or negotiate; they appear only through vicarious representation by delegations, advocacy groups, and doctrinal language such as the common-heritage clause. Everything they hold depends on decisions made decades before they can act.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Companies developing extraction and in-situ resource utilization technology for the Moon and asteroids, backed by venture and institutional capital that expects recoverable-property rights to anchor returns. Under the prohibition reading their business models lose their legal foundation: extracted material could not be owned, so capital deployed toward extraction is stranded. Their response has been jurisdictional - incorporation and licensing in states whose domestic statutes recognize ownership of recovered space resources - and political: funding advocacy for the rival interpretation. Exit for them means re-domiciling, not abandoning the industry.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, private_mining_ventures, payer,
    powerful, biographical, arbitrage, global).

% National industries in launch, orbital servicing, and planetary operations whose addressable markets widen if extracted resources become ownable and narrow if authorization is required first. They carry the compliance uncertainty directly: contract structures, insurance, and financing all price the risk that a multilateral rule retroactively invalidates resource titles. They lobby their governments toward permissive interpretations and build consortium structures that presume extraction rights.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_state_commercial_sectors, payer,
    powerful, generational, arbitrage, global).

% The UN body that maintains the interpretive record of the Outer Space Treaty, drafts resolutions, and convenes the state debate over space resource activities. It holds custody of the non-appropriation principle's meaning but commands no enforcement instrument: it cannot sanction a launching state, invalidate a domestic statute, or inspect a lunar facility. Its authority rests on being seen as the faithful steward of the 1967 text, which makes revisiting the principle's scope existentially costly - the body's standing is constituted by the continuity it administers.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, copuos_legal_subcommittee, agenda_setter,
    institutional, generational, identity_locked, global).

% A growing bloc of states, led by the United States, that have signed bilateral accords implementing a rival interpretation: extracted resources may be owned, and safety zones around operations are lawful. They stepped outside the consensus-based multilateral process precisely because that process would not endorse their reading, and they now operate a parallel normative track. They formally reaffirm the treaty's territorial clauses while declining its extension to resources - objection voiced from outside the room they were asked to stay in.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, artemis_accord_signatories, excluded,
    institutional, generational, mobile, global).

% Major spacefaring powers outside the accord bloc - principally the states behind the International Lunar Research Station partnership - that publicly criticize the rival accords as unilateral and call for a UN-centered multilateral framework, while themselves planning long-duration operations and resource use on the Moon. They gain from any rule that denies their competitor exclusive control, and they would pay under a binding authorization regime of their own making. Their position lets them shift between defending the commons principle and exercising capability, depending on which serves the program at hand.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, rival_program_major_powers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, rival_program_major_powers, payer).

% The academic and practitioner community that produces the interpretive arguments on which every camp draws. It holds no material stake and enforces nothing, but the contest sustains a substantial professional economy: chairs, journals, conference circuits, and advisory roles all feed on the unresolved question. Individual scholars attach careers to particular readings, which gives the community both its vitality and its incentive to keep the dispute alive.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, space_law_scholarship, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the open-access problem of celestial resources: without a prior-allocation rule, arrival order converts a shared domain into capability-based enclosure, inviting races, conflicting claims, and confrontation among powers able to project hardware. The reading holds the domain in common pending a multilateral allocation mechanism, giving every state a reason to accept delayed exploitation in exchange for guaranteed non-exclusion.
% TRANSFER_FUNCTION: Moves decision rights over celestial resources from whoever can physically extract to the collective of treaty parties, via a requirement of multilateral authorization; correspondingly moves expected resource value from first movers to a future negotiated distribution, and moves risk onto private ventures and national industries whose investments assume ownership.
% ABSENT_VOICES: Future generations hold the largest stake and have no seat except vicarious representation. Global South civil society and non-elite scientific users are effectively absent from the technical working groups where resource-activity rules are drafted. The rival-accords bloc removed itself from the consensus room and speaks from its parallel track. Would-be extractors are heard through lobbying rather than membership in the authorization conversation.
% DISAPPEARANCE_RATIONALE: If the prohibition reading vanished overnight - if every party agreed that extraction creates ownable title - the treaty's core bargain inverts: territorial non-appropriation was accepted by spacefaring states partly in exchange for free access, and resource enclosure would recreate de facto territoriality through operating footprints, safety zones, and supply-chain chokepoints. Allocation would reorganize around registries, licenses, and bilateral recognition instead of the multilateral framework; the non-spacefaring majority would lose its only lever; and the arms-control architecture that assumed no sovereign extension into space would face pressure at its central premise.
% FOUNDING_PROBLEM: The 1967 settlement answered a Cold War emergency: neither superpower would tolerate the other extending national territory - and the strategic posture that follows territory - to the Moon or planets. Article II closed the sovereignty question; the conservation reading extends the same answer to the resource question that reopened once extraction became technically plausible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the rival-accords bloc itself reaffirms Article II's territorial clause in the opening section of its own instruments, and permissive-reading scholarship concedes the anti-sovereignty core as settled. Cross-camp legal literature agrees the 1967 bargain solved territorial enclosure; the dispute is confined to whether its logic reaches resources. No party to the contest - including the parties this reading burdens - argues that the original sovereignty problem was illusory or resolved by other means.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.55: the prohibition imposes concentrated, real costs on capability-holders - stranded extraction capital, foregone national-industry upside, compliance uncertainty priced into contracts - while its protections disperse across the treaty community and forward in time; the reading's own lights legitimate these costs, so epsilon prices the asymmetry without pricing it as takings. Suppression 0.44 is authored as a raw structural property and is deliberately NOT scaled by power or scope at authoring - the engine owns that arithmetic: the reading coerces through interpretive authority, diplomatic isolation of violators, and denial of legal cover, not through inspection or sanction; it cannot stop the rival accords, and alternatives remain visibly open, which is why suppression sits below the midpoint. Theater 0.48: roughly half the reading's operative life is now declaratory maintenance - reaffirmation resolutions, benefit-sharing rhetoric without mechanism, scholarly rehearsal of settled positions - while the functional remainder is real: no state has claimed celestial territory, the territorial clause anchors every actor's legal argument, and the custodial record disciplines open annexation. Accessibility_collapse 0.25: understanding the reading does not close alternatives - the permissive path is codified in multiple domestic statutes and a large accord bloc, and jurisdictional arbitrage is a going concern. Resistance 0.70: the constraint meets organized, institutionalized resistance from the very states and industries it would bind, which is itself evidence it is a construct rather than a natural feature. All three temporal series run on one shared eight-point grid (1967-2026) so no metric borrows another's endpoints; the 2026 endpoints are authored projections for the closing year. The base_extractiveness series rises monotonically as extraction capability approaches feasibility - the accumulation signature is authored as data, not tuned to any verdict. Receipt surface: gains are authored diffuse - no named seat receives the extracted value; stranded capital dissipates rather than transferring, and the bloc's veto is positional standing, not receipt. Fixing cost is prohibitive: amendment requires consensus spanning all major powers and the non-spacefaring majority, and the permissive bloc works around the text rather than through it - circumvention at scale is itself evidence of fix cost.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the custodian seat should compute differently. From private_mining_ventures and spacefaring_state_commercial_sectors, the prohibition reads as confiscation of anticipated property: capital committed against a promise of title that the reading revokes. From nonspacefaring_states, the identical structure reads as the only barrier between them and permanent exclusion - protection they cannot buy anywhere else. The custodian experiences it as stewardship whose abandonment would be self-annihilating. The engine computes these divergent per-seat types from power, exit, and directional data; the authored claim does not adjudicate among them. Note also that the payer side has already coalitioned - industry capital fused with permissive-state diplomacy in the accord bloc - so the resistance metric reflects organized counter-coordination, not scattered complaint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: nonspacefaring_states, small_spacefaring_states, and future_generations sit near the subsidized end (low d), with future_generations furthest - trapped, powerless, unable to trade the benefit away. Victim declarations drive the opposite pole: private_mining_ventures and spacefaring_state_commercial_sectors sit near the full-target end, with arbitrage-grade exit (re-domiciling, licensing forum-shopping) pulling them slightly back from the trapped maximum. The custodian (copuos_legal_subcommittee) derives partial capture from its role: it administers the arrangement and its institutional standing is constituted by the kernel's stability, so it collects authority-rents a plain beneficiary reading would miss - but no per-power-atom override is authored, because an institutional-level override would also distort the rival-program and accord-bloc seats that share the atom; the role-and-exit data carry the distinction. Scope runs global-to-universal, which the engine folds into effective extraction at the verification-difficulty margin; suppression stays unscaled by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing sovereign extension into space - was solved at its territorial core and remains live at its resource edge, so no mandatrophy is declared: the mandate has extended, not expired. The classification guards both mislabelings. Reading the structure as pure extraction (the payer seats' experience) would erase the genuine coordination achievement: a sixty-year absence of celestial land rushes and territorial claims, bought at real but bounded cost. Reading it as pure coordination (the beneficiary and custodian framing) would erase the asymmetric burden and the custodian's self-interested stake in non-revision. The live risk is the third failure: if no enforcement pathway materializes, the reading completes its drift from operative norm to performed inheritance - theater_ratio crossing decisively above one-half, function carried by habit and custodial identity rather than effect - at which point the honest classification stops being contested coordination and becomes inertial maintenance. The enforcement_pathway_decay omega tracks exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Article II kernel governs celestial resource extraction - prohibition absent authorization (this reading), permissive private ownership, or deferral to a future regime?',
    'Binding adjudication (an ICJ advisory opinion or a dispute-settlement clause invoked by a party), or adoption of a multilateral resource regime that supersedes interpretive contest.',
    'If the permissive sibling prevails, this constraint''s victim and beneficiary sets invert and its epsilon collapses toward coordination cost; if the deferral sibling prevails, this constraint becomes provisional groundwork for the regime rather than an operative prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story instantiates the commons_conservation reading of kernel ost_article_ii_non_appropriation; sibling readings are separate constraints with their own epsilon and victim sets.').

omega_variable(
    private_actor_coverage_ambiguity,
    'Does Article II reach private actors directly - via Article VI''s state responsibility for national activities - such that a state licensing private extraction itself violates the non-appropriation principle?',
    'State-practice crystallization: whether licensing states are challenged as violating parties or treated as implementing authorities; eventual adjudication of a licensed-extraction dispute.',
    'If private actors are covered, domestic property statutes are violations and the target set widens to every licensing regime; if not, the principle binds state claims only and private ownership channels around it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_actor_coverage_ambiguity, empirical, 'Whether the principle''s coverage extends from states to private entities through Article VI supervision.').

omega_variable(
    enforcement_pathway_decay,
    'Can the prohibition hold without an enforcement regime, or does interpretive custody decay into declaratory performance as extraction capability matures?',
    'Track whether any authorization mechanism acquires compliance pull by 2035 - a revived Moon-Agreement-style regime, committee guidelines with reporting obligations, or lender and export-credit conditionality - versus continued resolution-only practice.',
    'Without a pathway, theater_ratio keeps climbing and the reading drifts toward inertial performance maintained by custodial habit; with one, the coordination-plus-burden structure stabilizes with enforceable teeth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_pathway_decay, empirical, 'Persistence question: normative authority alone versus enforcement machinery.').

omega_variable(
    stranding_magnitude_uncertainty,
    'How large are the real costs the prohibition imposes on capability-holders - does lunar or asteroid resource economics ever reach scales that make the stranded-investment burden material?',
    'Independent techno-economic assessment of in-situ resource utilization business cases; disclosure of invested capital at risk in extraction ventures.',
    'If extraction never approaches viability, the prohibition costs little and the structure reads closer to low-cost coordination; if large expected value rides on it, payer-seat pressure and resistance intensify sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stranding_magnitude_uncertainty, empirical, 'Magnitude of payer-seat costs depends on unsettled extraction economics.').

omega_variable(
    veto_value_reality,
    'Is the non-spacefaring bloc''s veto over enclosure real bargaining power, or positional standing that evaporates when a capable coalition simply proceeds?',
    'Observe whether any authorization demand by the majority bloc has altered a capable actor''s resource plans to date, and whether the accord-signing cascade erodes the bloc''s cohesion.',
    'If the veto is nominal, the beneficiary seat''s derived directionality overstates its collection and the structure''s asymmetry shifts toward the custodian and future generations; if real, the bloc is a genuine counterweight and negotiated distribution remains feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_value_reality, empirical, 'Reality-test for the beneficiary bloc''s collected benefit.').

omega_variable(
    cs_framing_underdetermination,
    'Is the constraint best framed as the prohibition norm itself (authority in the treaty text and its custodial interpretation), or as the custodial institution''s authority structure - which extracts institutional standing from preventing revision of the kernel?',
    'Classify under both framings and compare: the norm-framing yields the authored structure; the institution-framing routes the analysis through extraction-grounded authority and asks whether custodial standing survives loss of the resource question.',
    'Under the institution-framing, part of the measured persistence is authority-rent rather than normative force, raising effective extraction at the custodian seat and strengthening the case that the reading''s maintenance is partly self-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two defensible framings of the same arrangement yield different classifications; the story''s referent is the asserted norm, but custodial self-interest is visible in the drift record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost2commons_tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(ost2commons_tr_t1967, observed).
narrative_ontology:measurement(ost2commons_tr_t1979, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1979, 0.22).
narrative_ontology:measurement_basis(ost2commons_tr_t1979, observed).
narrative_ontology:measurement(ost2commons_tr_t1985, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1985, 0.3).
narrative_ontology:measurement_basis(ost2commons_tr_t1985, observed).
narrative_ontology:measurement(ost2commons_tr_t1998, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1998, 0.34).
narrative_ontology:measurement_basis(ost2commons_tr_t1998, observed).
narrative_ontology:measurement(ost2commons_tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(ost2commons_tr_t2015, observed).
narrative_ontology:measurement(ost2commons_tr_t2020, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(ost2commons_tr_t2020, observed).
narrative_ontology:measurement(ost2commons_tr_t2023, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2023, 0.46).
narrative_ontology:measurement_basis(ost2commons_tr_t2023, observed).
narrative_ontology:measurement(ost2commons_tr_t2026, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(ost2commons_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ost2commons_be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement_basis(ost2commons_be_t1967, observed).
narrative_ontology:measurement(ost2commons_be_t1979, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1979, 0.37).
narrative_ontology:measurement_basis(ost2commons_be_t1979, observed).
narrative_ontology:measurement(ost2commons_be_t1985, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1985, 0.39).
narrative_ontology:measurement_basis(ost2commons_be_t1985, observed).
narrative_ontology:measurement(ost2commons_be_t1998, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1998, 0.41).
narrative_ontology:measurement_basis(ost2commons_be_t1998, observed).
narrative_ontology:measurement(ost2commons_be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement_basis(ost2commons_be_t2015, observed).
narrative_ontology:measurement(ost2commons_be_t2020, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement_basis(ost2commons_be_t2020, observed).
narrative_ontology:measurement(ost2commons_be_t2023, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2023, 0.53).
narrative_ontology:measurement_basis(ost2commons_be_t2023, observed).
narrative_ontology:measurement(ost2commons_be_t2026, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(ost2commons_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost2commons_su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.28).
narrative_ontology:measurement_basis(ost2commons_su_t1967, observed).
narrative_ontology:measurement(ost2commons_su_t1979, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1979, 0.42).
narrative_ontology:measurement_basis(ost2commons_su_t1979, observed).
narrative_ontology:measurement(ost2commons_su_t1985, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1985, 0.36).
narrative_ontology:measurement_basis(ost2commons_su_t1985, observed).
narrative_ontology:measurement(ost2commons_su_t1998, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement_basis(ost2commons_su_t1998, observed).
narrative_ontology:measurement(ost2commons_su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.33).
narrative_ontology:measurement_basis(ost2commons_su_t2015, observed).
narrative_ontology:measurement(ost2commons_su_t2020, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(ost2commons_su_t2020, observed).
narrative_ontology:measurement(ost2commons_su_t2023, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2023, 0.41).
narrative_ontology:measurement_basis(ost2commons_su_t2023, observed).
narrative_ontology:measurement(ost2commons_su_t2026, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2026, 0.44).
narrative_ontology:measurement_basis(ost2commons_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, moon_agreement_article_xi_common_heritage).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'OST Article II non-appropriation' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (commons_conservation) authors epsilon for the prohibition arrangement as the conservation reading assesses it; the extraction_permissive sibling authors epsilon for a regime of ownable recovered resources with an inverted victim/beneficiary structure; the international_regime sibling authors epsilon for the deferral arrangement itself. The upstream claim (the treaty's territorial core, near-universally conceded) lends the downstream extensions their apparent authority, which is why the family links run from the settled core toward the contested extensions. The Moon Agreement's Article XI is the institutional descendant of this reading and is linked as a dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
