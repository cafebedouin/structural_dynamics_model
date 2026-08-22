% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__neutrality_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Non-Discrimination Requirement (Neutrality Reading)
 *   domain: technology governance/telecommunications law/internet policy
 *
 * SUMMARY:
 *   The neutrality reading of the tcp_ip_interpretation kernel holds that
 *   TCP/IP's end-to-end architecture embodies a non-discrimination
 *   requirement: access providers must carry packets without regard to
 *   content or application identity, edge innovators deploy without network
 *   permission, and ISP revenue optimization through differentiation is off
 *   the table. The constraint binds a fragmented jurisdictional patchwork —
 *   EU Regulation 2015/2120, California's SB 822, a re-adopted and litigated
 *   federal order in the United States — against two decades of sustained ISP
 *   opposition. Per the epsilon-invariance discipline, this file instantiates
 *   ONE reading only: the sibling readings (prioritization_reading,
 *   zero_rating_reading) are separate constraints with their own epsilon and
 *   beneficiary/victim structures, linked through network.affects_constraints
 *   and cs_structure.reading_relations. The architectural fact that TCP/IP
 *   ships no built-in QoS mechanism is a further distinct constraint — a
 *   design fact, not a requirement — and is not classified here. Note the
 *   tension this reading itself carries: its rhetoric has mountain form ('the
 *   architecture embodies the principle'), while the constraint's actual
 *   operation is enforced, contested, and beneficiary-bearing; the claim and
 *   the metrics below are authored independently, and the engine measures
 *   that divergence. KEY AGENTS (by structural relationship): -
 *   broadband_isps: Primary target (institutional/constrained) — bears the
 *   constraint's costs; two-decade litigant against every codification -
 *   edge_application_providers: Primary beneficiary
 *   (institutional/constrained) — reach every user through neutral last-mile
 *   without termination fees - internet_end_users: Beneficiary with diffuse
 *   indirect costs (organized/constrained) — open access; limited provider
 *   choice - startup_edge_developers: Protected beneficiary
 *   (powerless/constrained) — deploy without network permission -
 *   open_internet_regulators: Agenda setter (institutional/constrained) —
 *   administer the non-discrimination requirement across a fragmented
 *   patchwork - zero_rating_sponsors: Excluded voice (institutional/trapped)
 *   — would sponsor content; barred by the constraint's operation -
 *   internet_architecture_community: Analytical observer
 *   (analytical/analytical) — custodian of the end-to-end design tradition
 *
 * KEY AGENTS:
 *   - broadband_isps: Primary target (institutional/constrained) — bears the constraint's costs; sustained litigant
 *   - edge_application_providers: Primary beneficiary (institutional/constrained) — neutral last-mile reach without termination fees
 *   - internet_end_users: Beneficiary with diffuse indirect costs (organized/constrained) — open access, limited provider choice
 *   - startup_edge_developers: Protected beneficiary (powerless/constrained) — permissionless deployment
 *   - open_internet_regulators: Agenda setter (institutional/constrained) — enforcement across a fragmented patchwork
 *   - zero_rating_sponsors: Excluded voice (institutional/trapped) — barred from the transaction the constraint prohibits
 *   - internet_architecture_community: Analytical observer (analytical/analytical) — design-tradition custodian, no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.42).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.5).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Non-Discrimination Requirement (Neutrality Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology governance/telecommunications law/internet policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'c015e5a8-3c54-428e-8d6f-f275ca8e543e').
narrative_ontology:cs_kernel_codification('c015e5a8-3c54-428e-8d6f-f275ca8e543e', fixed_text).
narrative_ontology:cs_authority_grounding('c015e5a8-3c54-428e-8d6f-f275ca8e543e', lineage).
narrative_ontology:cs_interpretation_layer_present('c015e5a8-3c54-428e-8d6f-f275ca8e543e').
narrative_ontology:cs_reading_relation('c015e5a8-3c54-428e-8d6f-f275ca8e543e', tcp_ip_interpretation__prioritization_reading, forecloses).
narrative_ontology:cs_reading_relation('c015e5a8-3c54-428e-8d6f-f275ca8e543e', tcp_ip_interpretation__zero_rating_reading, forecloses).
narrative_ontology:cs_axiom('c015e5a8-3c54-428e-8d6f-f275ca8e543e', foundational, end_to_end_requires_nondiscrimination).
narrative_ontology:cs_axiom_status(end_to_end_requires_nondiscrimination, holdable).
narrative_ontology:cs_axiom_grounding('c015e5a8-3c54-428e-8d6f-f275ca8e543e', end_to_end_requires_nondiscrimination, instrumental).
narrative_ontology:cs_axiom('c015e5a8-3c54-428e-8d6f-f275ca8e543e', foundational, permissionless_edge_innovation).
narrative_ontology:cs_axiom_status(permissionless_edge_innovation, holdable).
narrative_ontology:cs_axiom_grounding('c015e5a8-3c54-428e-8d6f-f275ca8e543e', permissionless_edge_innovation, deontological).
narrative_ontology:cs_reference_frame('c015e5a8-3c54-428e-8d6f-f275ca8e543e', end_to_end_neutral_transport).
narrative_ontology:cs_drift_state('c015e5a8-3c54-428e-8d6f-f275ca8e543e', contemporary_post_2017_repeal, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c015e5a8-3c54-428e-8d6f-f275ca8e543e', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_application_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_end_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, startup_edge_developers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, broadband_isps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, broadband_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, internet_end_users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_design_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, permissionless_innovation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate fixed and mobile access networks; sell subscriber connectivity as their revenue base. The constraint bars them from charging edge providers for prioritized delivery, from exempting sponsored content from metered plans, and from blocking or throttling by application. They have litigated every codification (Verizon v. FCC, USTelecom v. FCC, Mozilla v. FCC), won a federal repeal in 2017, and remain bound by EU regulation, California law, and a re-adopted federal order. Sunk plant and licensed spectrum make exit from the markets they serve impractical; their incidental stake in a rich edge ecosystem — which drives subscription demand — is the benefit side of their dual position.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, broadband_isps, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, broadband_isps, beneficiary).

% Deliver content and applications over ISP last-mile infrastructure to end users without paying ISPs for termination or prioritization. Encryption, CDNs, and their own long-haul infrastructure give them leverage in the middle of the network but none at the access layer, where the constraint is what keeps their reach permissionless. They were the constraint's principal advocates in its codification fights.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_providers, beneficiary,
    institutional, generational, constrained, global).

% Reach any lawful content and run any application without ISP permission or application-based degradation. They pay subscription prices that are the ISPs' compensation for carriage, and bear indirectly whatever compliance and capacity costs ISPs pass through. Most broadband markets offer one or two viable providers, so their exit from any given access arrangement is narrow.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, internet_end_users, payer).

% Deploy new applications to the whole addressable market without negotiating with each access network — the permissionless entry the constraint protects. They have no leverage over ISPs and no alternative route to users' screens; their access depends entirely on the constraint holding.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, startup_edge_developers, beneficiary,
    powerless, biographical, constrained, global).

% Administer and enforce the non-discrimination requirement across a fragmented patchwork: EU national regulatory authorities and BEREC under Regulation 2015/2120, California's enforcement of SB 822, and a re-adopted federal order in litigation in the United States. They write the guidance that draws the management/discrimination boundary, hear complaints, and issue orders; their authority flips with political turnover, which is why enforcement intensity oscillates.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, open_internet_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Platforms and carriers that would fund sponsored-content exemptions — free or zero-rated access to selected services in exchange for sponsorship. The constraint bars the model in every jurisdiction where it binds; they argue their case through the zero-rating reading rather than inside this arrangement, and have no seat in its administration.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, zero_rating_sponsors, excluded,
    institutional, biographical, trapped, global).

% The IETF, ISOC, and the research community that authored and maintains the end-to-end design tradition. They articulate what the architecture embodies, publish the design arguments both sides cite, and hold no enforcement power; their standing rests on competence and lineage, not jurisdiction.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_architecture_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the access layer application-agnostic so that any edge innovator can deploy to the whole addressable market without network permission and every user can reach any lawful content — solving the gatekeeping problem that would otherwise make each ISP the toll-taker and arbiter of what the application layer may offer.
% TRANSFER_FUNCTION: Constrains ISP revenue optimization: value ISPs could otherwise capture by selling prioritized delivery, sponsored exemptions, or edge termination is left uncollected, and the capacity burden of edge traffic growth remains with subscriber-funded ISP networks rather than being surcharged to edge providers.
% ABSENT_VOICES: Sponsored-content sponsors and would-be paid-prioritization purchasers have no seat — the constraint exists to bar their transaction, so they argue through the sibling readings (zero_rating_reading, prioritization_reading) rather than inside this arrangement. ISPs hold seats only as payers; their claim that carriage is uncompensated is heard in litigation and rulemaking comments, not as a co-authoring voice in the constraint's administration.
% DISAPPEARANCE_RATIONALE: If the non-discrimination requirement vanished overnight, access providers would roll out prioritized tiers and sponsored-data plans within product cycles, edge startups would face termination negotiations before launch, incumbent edge platforms would pay for assured delivery and pass costs on, and the application layer would reorganize around ISP gatekeeping — the open-edge ecosystem this arrangement holds in place would rearrange around paid access.
% FOUNDING_PROBLEM: Access-provider gatekeeping at the dawn of consumer broadband: a regional ISP blocked competing VoIP (Madison River, 2005) and a national cable operator throttled peer-to-peer traffic (Comcast, 2007), demonstrating that once capacity constraints and business incentives aligned, access providers could and would discriminate among applications. The constraint was built to keep the access layer from becoming the application layer's gatekeeper.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the FCC's Madison River consent decree (2005) and Comcast-BitTorrent order (2008) attest the discrimination problem from the enforcement seat; the recitals of EU Regulation 2015/2120 attest it from the legislative seat; ISPs' own transparency disclosures attest the capability from the payer side. The ISPs and the 2017 FCC attest the opposite on salience — that competition and traffic growth dissolved the problem — which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).
:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42: the constraint imposes real, asymmetric burdens on ISPs — compliance machinery, capacity investment it cannot surcharge to edge providers, and the foregone revenue of prioritization and sponsored-data markets — but from this reading's own lights those burdens are legitimate carriage costs compensated by subscriber revenue, not confiscation; the moderate value holds both facts honestly. Suppression 0.50: the constraint holds only through active enforcement — orders, fines, litigation defense — against sustained ISP resistance; it suppresses ISP practices, not user or edge alternatives, and is authored as the raw structural property it is (the engine, not the author, scales extraction by directionality and scope). Theater 0.40: a binding core in enforced jurisdictions coexists with voluntary pledges, transparency-report rituals, and merger-condition promises where enforcement collapsed. Accessibility collapse 0.35: the prioritization and zero-rating alternatives remain live and partially implemented — this constraint does not collapse them. Resistance 0.75: two decades of litigation (Verizon v. FCC, USTelecom v. FCC, Mozilla v. FCC), a complete federal repeal in 2017, and continuous lobbying. The measurement series run on one shared grid (0/4/8/12/16/20) and show a ratchet-and-release cycle — enforcement built to a 2017 peak, released by repeal, partially re-built since — driven by administrative turnover rather than by the constraint's internal dynamics; the oscillation is a side effect of political cycling, not an intermittent-reinforcement mechanism. base_properties values are end-state (T=20).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute different types from identical structure. From the ISP seat the arrangement is the removal of a revenue option it never consented to surrender, enforced by regulators it cannot exit; from the edge and user seats the same structure is the protective condition of a permissionless application layer; from the regulator seat it is an administrative mandate whose management-versus-discrimination boundary it must draw case by case. The engine computes this per-seat divergence from the structural data; this story's claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge providers, end users, and startup developers are declared beneficiaries: d sits near the beneficiary end and effective extraction damps or inverts for them. Broadband ISPs are declared victims (role: payer) with constrained exit (sunk plant, licensed spectrum) and national scope: d sits near the full-target end and effective extraction amplifies. Their secondary beneficiary position — the open edge drives subscription demand — is real but does not move them off the target end, because the constraint's costs land on them directly regardless of ecosystem effects. Open internet regulators administer without materially benefiting or paying, sitting near symmetric. Zero-rating sponsors are structurally barred and highly exposed to the constraint's prohibition, but they appear in no beneficiary/victim array, so their directionality rides the derivation fallback; no override is authored because the coarse power-atom override surface cannot distinguish them from other institutional actors. Scope amplification flows through the declared scopes (global edge, national ISPs, continental regulators); suppression is not scope-scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — access-provider gatekeeping — is contested, not dead: ISPs retain both the capability (deep packet inspection, network slicing) and the incentive, but the parties dispute its salience and the 2017 FCC formally found it insufficient to warrant rules. founding_problem_status=contested with disappearance_verdict=world_rearranges is the honest pairing: the world does depend on the arrangement, and whether its reason has expired is precisely what the kernel contest disputes — so the mismatch consumer should see a live dispute, not a zombie flag. The classification prevents two misreadings: calling the constraint a snare (pure extraction on ISPs) erases the genuine coordination good — the permissionless edge — that even ISP-side analyses concede exists; calling it a rope (net beneficiaries all around) erases the ISPs' real, non-consented, asymmetric burden and the enforcement machinery needed to hold it against two decades of their resistance. Tangled rope holds both halves. The mandatrophy risk is jurisdictional rather than global: where enforcement collapsed, the norm persists partly as pledge and ritual — the fragmented_regime_enforcement omega tracks whether those jurisdictions are piton-trending.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the tcp_ip_interpretation kernel — the neutrality_reading. What would structurally change if a sibling reading (prioritization_reading, zero_rating_reading) were adopted as the kernel''s operative interpretation?',
    'Adoption of a sibling reading as governing law — statute, repeal-and-replace rulemaking, or controlling appellate interpretation — observable as an inversion of the constraint''s victim/beneficiary sets rather than a re-measurement of this story.',
    'Under prioritization_reading, ISPs become beneficiaries of a differentiated-service market and edge providers become payers; under zero_rating_reading, sponsors become beneficiaries and unsponsored content becomes disadvantaged. This story''s epsilon, victim set, and classification would not survive — the sibling files instantiate those structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel reading this story instantiates, and what sibling adoption would change.').

omega_variable(
    discrimination_management_boundary,
    'Where does prohibited content/application-based discrimination end and legitimate reasonable network management begin — and do application-class differentiation (video vs. bulk transfer) and 5G network slicing fall on the prohibition side?',
    'Case-by-case adjudication against technical measurement: BEREC guidelines and FCC advisory opinions drawing the boundary, plus traffic analysis of whether differentiation tracks application identity or network state.',
    'A wide management zone shrinks the constraint''s bite and lowers effective extraction on ISPs; a narrow zone expands both and hardens the ISP seat''s experience toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discrimination_management_boundary, conceptual, 'The boundary locus where the kernel''s readings actually disagree.').

omega_variable(
    isp_compensation_adequacy,
    'Does subscriber revenue compensate ISPs for carrying edge traffic under neutrality, or does unconstrained edge traffic growth impose uncompensated marginal costs the constraint forces ISPs to absorb?',
    'Access-network cost accounting: marginal cost of peak edge traffic versus subscriber revenue, and investment trends under neutral versus differentiated regimes across jurisdictions.',
    'If carriage is uncompensated at the margin, the constraint''s burden on ISPs is heavier than authored and the ISP seat computes toward pure extraction; if compensated, the coordination component dominates and the tangled_rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isp_compensation_adequacy, empirical, 'Whether the ISPs'' alleged uncompensated carriage burden is real or a rent-seeking frame.').

omega_variable(
    fragmented_regime_enforcement,
    'Post-2017, does the constraint persist as binding law in each jurisdiction (EU, California, litigated federal re-adoption) or as theatrical voluntary pledge — and are pledge-only jurisdictions piton-trending?',
    'Enforcement docket analysis: complaints, orders, and penalties under Regulation 2015/2120 and SB 822, versus pledge violations without consequence in pledge-only jurisdictions.',
    'If theatrical where unenforced, the constraint is a patchwork — tangled_rope in enforced jurisdictions, piton-trending in pledge-only ones — and per-seat classifications diverge by jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmented_regime_enforcement, empirical, 'Binding core versus theatrical maintenance across the fragmented jurisdictional patchwork.').

omega_variable(
    edge_incumbency_feedback,
    'The constraint was built to protect permissionless edge innovation; its largest beneficiaries are now incumbent platforms with the scale to exploit open access. Does neutrality still function as startup protection, or as an incumbent moat that ISPs cannot toll?',
    'Edge traffic-share and startup-formation data under neutral versus differentiated regimes; whether new entrants still reach users without incumbent intermediation.',
    'If incumbent-moat, the coordination good concentrates in a few seats and the beneficiary structure shifts — ISPs pay, incumbents collect — moving gain_flow off ''diffuse'' and hardening the extraction asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(edge_incumbency_feedback, empirical, 'Whether the protected edge remains permissionless in practice or has concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_neutrality_tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t4, tcp_ip_interpretation__neutrality_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t4, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t8, tcp_ip_interpretation__neutrality_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t8, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t12, tcp_ip_interpretation__neutrality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t12, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t16, tcp_ip_interpretation__neutrality_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t16, observed).
narrative_ontology:measurement(tcp_ip_neutrality_tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tcp_ip_neutrality_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(tcp_ip_neutrality_be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t4, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t4, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t8, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t8, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t12, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t12, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t16, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t16, observed).
narrative_ontology:measurement(tcp_ip_neutrality_be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(tcp_ip_neutrality_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_neutrality_su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t0, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t4, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t4, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t8, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t8, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t12, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t12, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t16, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t16, observed).
narrative_ontology:measurement(tcp_ip_neutrality_su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(tcp_ip_neutrality_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'network neutrality / what TCP/IP requires' conflates three structurally distinct claims about one kernel (tcp_ip_interpretation). This file instantiates the neutrality reading only: a non-discrimination requirement on ISPs, with edge providers and users as beneficiaries and ISPs as payers. The prioritization reading (differentiated service quality as management) and the zero-rating reading (sponsored exemptions) are separate stories with their own epsilon and inverted payer/beneficiary sets. The readings disagree about one locus: whether application-identity-based packet treatment is ever legitimate management. The architectural fact that TCP/IP ships no QoS mechanism is a further distinct constraint — a design fact, not a requirement — and is not classified here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
