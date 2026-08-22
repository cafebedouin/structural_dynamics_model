% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Network Neutrality Reading of TCP/IP End-to-End Principle
 *   domain: technology governance/internet policy/telecommunications law
 *
 * SUMMARY:
 *   This story instantiates the neutrality reading of the contested TCP/IP
 *   kernel: the claim that the end-to-end design principle embeds an
 *   enforceable non-discrimination norm binding on last-mile ISPs. Under this
 *   reading, TCP/IP's architecture is not merely a technical substrate but a
 *   normative commitment that forecloses content- or application-based
 *   traffic discrimination. This is one of three sibling constraints sharing
 *   the kernel_id tcp_ip_interpretation — the prioritization_reading
 *   (permitting differentiated service quality as network management) and the
 *   zero_rating_reading (permitting selective sponsored-content exemptions)
 *   are separate constraint stories, not alternative measurements of this
 *   one. Each carries its own epsilon and stakeholder structure; this file
 *   does not average across them or hedge its extraction value to accommodate
 *   their claims.
 *
 * KEY AGENTS:
 *   - edge_application_developers: primary beneficiary (moderate/mobile) — builds without carrier permission
 *   - content_startups: primary beneficiary (powerless/mobile) — depends on flat-rate carriage
 *   - public_interest_internet_advocates: agenda_setter (organized/analytical) — mobilizes the reading politically
 *   - last_mile_isps: primary target (institutional/constrained) — barred from prioritization revenue
 *   - vertically_integrated_telecom_carriers: secondary target (institutional/constrained) — barred from vertical leverage
 *   - telecommunications_regulators: analytical observer (institutional/analytical) — adjudicates between readings
 *   - rural_and_underserved_users: excluded voice (powerless/trapped) — stakes without standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.42).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "Network Neutrality Reading of TCP/IP End-to-End Principle").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology governance/internet policy/telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'db272692-cebb-4e8a-a7cb-681478ab1697').
narrative_ontology:cs_kernel_codification('db272692-cebb-4e8a-a7cb-681478ab1697', distributed).
narrative_ontology:cs_authority_grounding('db272692-cebb-4e8a-a7cb-681478ab1697', distributed).
narrative_ontology:cs_reading_relation('db272692-cebb-4e8a-a7cb-681478ab1697', tcp_ip_interpretation__prioritization_reading, forecloses).
narrative_ontology:cs_reading_relation('db272692-cebb-4e8a-a7cb-681478ab1697', tcp_ip_interpretation__zero_rating_reading, forecloses).
narrative_ontology:cs_axiom('db272692-cebb-4e8a-a7cb-681478ab1697', foundational, non_discrimination_is_architecturally_mandatory).
narrative_ontology:cs_axiom_status(non_discrimination_is_architecturally_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('db272692-cebb-4e8a-a7cb-681478ab1697', non_discrimination_is_architecturally_mandatory, conventional).
narrative_ontology:cs_axiom('db272692-cebb-4e8a-a7cb-681478ab1697', secondary, edge_innovation_requires_permission_free_carriage).
narrative_ontology:cs_axiom_status(edge_innovation_requires_permission_free_carriage, holdable).
narrative_ontology:cs_axiom_grounding('db272692-cebb-4e8a-a7cb-681478ab1697', edge_innovation_requires_permission_free_carriage, instrumental).
narrative_ontology:cs_reference_frame('db272692-cebb-4e8a-a7cb-681478ab1697', end_to_end_architectural_neutrality).
narrative_ontology:cs_drift_state('db272692-cebb-4e8a-a7cb-681478ab1697', post_2017_repeal_and_reinstatement_cycle, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db272692-cebb-4e8a-a7cb-681478ab1697', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_application_developers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_startups).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, public_interest_internet_advocates).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, last_mile_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, vertically_integrated_telecom_carriers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, end_to_end_design_principle).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__neutrality_reading, innovation_without_permission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build services that ride the network without needing carrier permission or paid prioritization. Under this reading, ISPs may not degrade, block, or charge them for preferential treatment, so a new entrant's traffic reaches users on the same technical footing as an incumbent's. Their exit option is genuinely mobile — they can build on any compliant network without negotiating individually with each carrier.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Depend on non-discriminatory carriage to reach an audience without paying interconnection or prioritization fees they cannot afford. This reading treats their access as protected by the same technical grammar that protects incumbents, converting what could be a toll relationship into a flat-rate one.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_startups, beneficiary,
    powerless, biographical, mobile, national).

% Litigate, lobby regulators, and file comments arguing that end-to-end non-discrimination is what TCP/IP's architecture always meant, and that any deviation is a policy choice dressed as technical necessity. They set the interpretive agenda for this reading by mobilizing it into rulemakings and court challenges; they collect no rent but administer the reading's political life.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, public_interest_internet_advocates, agenda_setter,
    organized, generational, analytical, national).

% Own the physical last-mile infrastructure and would prefer to monetize differentiated service tiers, paid prioritization, and sponsored-data arrangements. Under this reading they are barred from doing so on content or application grounds; their exit option is constrained because relinquishing their network footprint is not viable, but they can lobby for reclassification or wait for a change in the presiding regulatory framework.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, last_mile_isps, payer,
    institutional, generational, constrained, national).

% Own both network infrastructure and content or streaming properties, and would benefit from prioritizing their own vertical offerings over rivals. This reading forecloses that revenue strategy for the interconnection layer, constraining their ability to leverage network ownership into content-market advantage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, vertically_integrated_telecom_carriers, payer,
    institutional, generational, constrained, continental).

% Adjudicate which reading of TCP/IP's design commitments governs enforceable rules, weighing competing technical and economic testimony. Their rulings determine whether this reading, the prioritization reading, or the zero-rating reading becomes binding in a given jurisdiction, and they can reverse course when political administrations change.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecommunications_regulators, observer,
    institutional, generational, analytical, national).

% Would benefit from whichever reading actually expands access and lowers cost in their specific market, but are rarely direct parties to the interpretive contest — their interests are represented, if at all, by advocacy organizations rather than by their own testimony. They have no meaningful exit from a single available ISP regardless of which reading prevails.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, rural_and_underserved_users, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single technical and interpretive standard for packet handling — no ISP degrades or prioritizes traffic by content or application identity — so that any edge innovator can build and deploy without individually negotiating carriage terms with every network operator.
% TRANSFER_FUNCTION: Moves the option value of differentiated pricing and prioritization away from network owners (who cannot monetize it) and toward edge developers and content originators (who receive uniform best-effort carriage as a default entitlement, effectively subsidized by the network's ordinary revenue rather than paying for it directly).
% ABSENT_VOICES: Rural and underserved end users, whose actual access and price outcomes are the ostensible stakes of the dispute, rarely testify directly in the rulemakings and litigation that adjudicate between readings; their interests are proxied by advocacy groups and by carriers claiming to represent them.
% DISAPPEARANCE_RATIONALE: If this reading vanished and a different reading of the kernel became controlling, last-mile ISPs would gain a differentiated-pricing revenue channel and could reallocate capital toward paid prioritization infrastructure; edge developers and startups dispute whether this would improve network investment (the carriers' claim) or entrench incumbent content platforms able to pay for prioritization (the advocates' claim) — the parties disagree about which world is better, not merely about which world would exist.
% FOUNDING_PROBLEM: Early internet architecture needed a design principle that let innovation happen at the edges without requiring permission from network operators in the middle — solving the problem of how a decentralized, general-purpose network could support applications its designers never anticipated.
% FOUNDING_PROBLEM_CORROBORATION: Original protocol architects (e.g. testimony and writings from IETF-era engineers) attest that end-to-end design was a genuine technical commitment, not merely a policy overlay; telecom carriers and some network economists, outside the advocacy coalition that benefits from this reading, contend the end-to-end principle was always compatible with reasonable network management and that the strict non-discrimination gloss is a later political interpretation layered onto a narrower technical design choice.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, contested).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 because under this reading, the constraint transfers a real economic asset — the option to price-discriminate on prioritization — from network owners to edge actors, without compensating the network owners for the foregone revenue; this is a genuine transfer, not merely a coordination cost. Suppression is moderate (0.42) because enforcement (net neutrality rules, FCC orders, EU regulations) actively forecloses ISP business models rather than merely disincentivizing them, but the suppression is bounded by shifting political administrations and periodic reclassification battles rather than being a stable, high-suppression regime. Theater ratio rose gradually (0.12 to 0.28) as neutrality rules were repeatedly adopted, repealed, and readopted across jurisdictions and administrations, producing increasing volumes of comment-period and litigation activity relative to periods of stable enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (edge developers, startups), this reading looks like Rope — a genuine coordination solution that lets innovation happen without gatekeeping. From the payer seats (last-mile ISPs, vertically integrated carriers), the identical structure looks like enforced extraction of their pricing option value, dressed in the language of technical necessity. The engine's per-seat computation should reflect this divergence: the same enforced non-discrimination rule generates different effective extraction values depending on which side of the transfer an agent sits on.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge developers and content startups are declared beneficiaries because the constraint removes a cost they would otherwise bear (prioritization fees) and protects an option they would otherwise lack (guaranteed carriage) — this pushes their directionality toward the beneficiary end. Last-mile ISPs and vertically integrated carriers are declared victims because the constraint removes a revenue option they structurally possess by virtue of owning the last-mile infrastructure — this pushes their directionality toward the target end, especially given their constrained exit (sunk infrastructure investment, no plausible exit from the regulated jurisdiction). Public interest advocates are agenda-setters rather than beneficiaries because they collect no direct rent from the arrangement; they administer and mobilize the reading's political viability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permission-free edge innovation) remains genuinely contested as either live or resolved: edge innovation continues at a scale that arguably no longer requires the specific non-discrimination guarantee (platforms like large content delivery networks now negotiate direct interconnection deals that function similarly to prioritization), suggesting to critics that this reading has drifted from solving an active problem to defending an entrenched coalition's revenue model. Advocates dispute this, arguing new entrants without CDN-scale bargaining power still depend on the baseline guarantee. Classifying this as tangled_rope rather than pure rope or pure snare prevents both over-crediting the reading as costless coordination and over-condemning it as pure extraction — it genuinely coordinates permission-free innovation while genuinely constraining a real revenue option from network owners, and both are simultaneously true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    architectural_commitment_vs_policy_overlay,
    'Does the end-to-end principle, as originally specified in TCP/IP''s design documents and early IETF practice, constitute a binding normative commitment to non-discrimination, or was it a narrower technical design choice about where to locate complexity (at the edges) that later advocacy generalized into a policy mandate?',
    'Historical analysis of founding RFCs, IETF working group records, and contemporaneous engineering commentary (e.g. Saltzer, Reed, and Clark''s original end-to-end papers) cross-referenced against how the principle was actually invoked in early network operation disputes, before the network neutrality policy debate existed.',
    'If the architectural reading is correct, this reading''s claim to represent the ''true'' meaning of TCP/IP is strengthened and the sibling readings are revealed as later policy impositions on a settled technical fact. If the narrower reading is correct, this reading is itself a policy choice with a technical veneer, structurally symmetrical to its siblings rather than privileged by original design intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_commitment_vs_policy_overlay, conceptual, 'Whether end-to-end non-discrimination was an original architectural commitment or a retrospective policy gloss.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that three structurally distinct readings (neutrality, prioritization, zero-rating) all claim fidelity to the same TCP/IP kernel, what determines which reading becomes legally binding in a given jurisdiction at a given time — technical merit, political coalition strength, judicial precedent, or some combination?',
    'Comparative jurisdictional analysis: track which reading prevailed in each major regulatory reversal (US 2015 Open Internet Order, 2017 repeal, 2024 reinstatement attempt; EU 2015 Telecoms Single Market Regulation) against contemporaneous political administration, court composition, and lobbying expenditure data.',
    'If political coalition strength dominates, none of the three readings can claim to be the ''correct'' interpretation of the kernel in any technical sense — all three are policy choices contesting the same ambiguous text, and this reading''s authority rests entirely on which coalition currently holds regulatory power, not on architectural fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'What actually determines which kernel reading becomes the binding legal interpretation.').

omega_variable(
    beneficiary_coalition_stability,
    'Are edge_application_developers and content_startups a stable, unified beneficiary coalition, or does the coalition fracture once some edge actors (large streaming platforms, CDN operators) reach a scale where they would benefit from prioritization arrangements themselves?',
    'Track the position of large content platforms in neutrality rulemakings over time — do they consistently support strict non-discrimination, or do they begin advocating for negotiated interconnection arrangements once they have scale?',
    'If large edge actors defect from the neutrality coalition once they reach scale, the beneficiary group is not stable and the reading''s coordination function (protecting all edge innovation equally) is partial rather than universal — it protects small/new entrants but not the platforms it originally protected once they mature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_coalition_stability, empirical, 'Whether the edge-developer beneficiary coalition remains stable as its members grow to scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 2003, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2003, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement(tcp__tr_t2007, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2007, 0.16).
narrative_ontology:measurement(tcp__tr_t2011, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(tcp__tr_t2019, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2003, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(tcp__be_t2007, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2007, 0.4).
narrative_ontology:measurement(tcp__be_t2011, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2011, 0.46).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(tcp__be_t2019, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2003, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2003, 0.2).
narrative_ontology:measurement(tcp__su_t2007, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2007, 0.28).
narrative_ontology:measurement(tcp__su_t2011, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2011, 0.34).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(tcp__su_t2019, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.1).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the colloquial 'net neutrality debate' / 'TCP/IP end-to-end principle' label per the epsilon-invariance principle. neutrality_reading (this file) claims epsilon=0.58 under a tangled_rope classification: it coordinates permission-free edge innovation while extracting a real revenue option from last-mile ISPs. prioritization_reading and zero_rating_reading are separate files with their own epsilon values and stakeholder structures, reflecting the fact that these are structurally distinct legal-technical claims about the same underlying protocol architecture, not three measurements of one constraint. All three share the kernel_id tcp_ip_interpretation and are linked bidirectionally via affects_constraints so contamination propagation analysis can trace how a purity shift in one reading (e.g. a court ruling undermining its legal basis) affects the political viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
