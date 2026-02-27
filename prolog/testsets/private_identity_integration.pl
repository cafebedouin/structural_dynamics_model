% ============================================================================
% CONSTRAINT STORY: private_identity_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_identity_integration, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: private_identity_integration
 *   human_readable: The Closed-Door Identity Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The closed-door identity protocol is a social constraint that mandates
 *   privacy for identity integration over a period of years. It
 *   simultaneously solves a real coordination problem (how to internalize new
 *   self-understanding without external judgment destabilizing the process)
 *   and extracts value from excluded networks, digital accountability
 *   systems, and therapeutic practitioners who gatekeep the timeline. The
 *   constraint exhibits all six DR types depending on the observer's
 *   structural position. From the integrator's perspective, it is
 *   coordination (Rope) — temporary privacy enables authentic exploration.
 *   From the social network's perspective, it is pure extraction (Snare) —
 *   they are locked out of witnessing or validating the transformation and
 *   cannot exit. From the therapist's perspective, it is mixed coordination
 *   and extraction (Tangled Rope) — they enable safe integration but also
 *   monopolize access to the authentic self and control the re-opening
 *   timeline. The institutional therapy-industrial complex sees it as
 *   profitable coordination (Tangled Rope). The psychological authenticity
 *   ritual sees itself as degraded but persistent (Piton). The analytical
 *   observer risks naturalizing a contingent cultural practice as an
 *   immutable law of human development (false summit / Mountain). The
 *   constraint's theater ratio (0.61) reflects that much of its enforcement
 *   relies on narrative ('you're not ready yet,' 'authenticity takes time')
 *   rather than clear functional boundaries.
 *
 * KEY AGENTS:
 *   - Identity Integrator (Self-Directed): Primary beneficiary (institutional/arbitrage) — controls timing of disclosure, frames their transformation narrative, captures social validation on their own schedule
 *   - Excluded Social Networks: Primary victim (powerless/trapped) — locked out of identity formation process, must absorb behavioral inconsistency and unexplained social withdrawal without explanation or participation
 *   - Digital Accountability Systems: Primary victim (powerless/trapped) — public records remain static and falsely coherent; cannot adapt to the private identity transformation; maintain obsolete persona documentation
 *   - Therapeutic Practitioners: Secondary beneficiary and enforcer (moderate/constrained) — benefit from privileged access to unguarded introspection and gatekeeping authority; bound by confidentiality ethics and professional standards
 *   - Institutional Therapy-Industrial Complex: Secondary beneficiary (powerful/arbitrage) — profit from service provision; incentives align with protocol perpetuation and prolonged engagement timelines
 *   - Psychological Authenticity Ritual: Institutional actor maintaining theater (institutional/arbitrage) — performs legitimacy through pop-psychology frames; persists through inertia despite weak functional justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_identity_integration, 0.38).
domain_priors:suppression_score(private_identity_integration, 0.52).
domain_priors:theater_ratio(private_identity_integration, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_identity_integration, extractiveness, 0.38).
narrative_ontology:constraint_metric(private_identity_integration, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(private_identity_integration, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_identity_integration, tangled_rope).
narrative_ontology:human_readable(private_identity_integration, "The Closed-Door Identity Protocol").
narrative_ontology:topic_domain(private_identity_integration, "social/psychological").

domain_priors:requires_active_enforcement(private_identity_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_identity_integration, identity_integrators).
narrative_ontology:constraint_beneficiary(private_identity_integration, therapeutic_practitioners).
narrative_ontology:constraint_victim(private_identity_integration, external_social_networks).
narrative_ontology:constraint_victim(private_identity_integration, digital_accountability_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED SOCIAL NETWORK (SNARE) — Friends, family, colleagues, and online communities are structurally locked out of witnessing or validating identity formation processes. They bear the cost of behavioral inconsistency, sudden social withdrawal, or unexplained shifts in values and interests. Cannot exit or reorganize around the hidden integration process. Maximum extraction: absorb confusion and disconnection while the integrator redesigns their identity privately.
constraint_indexing:constraint_classification(private_identity_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIGITAL ACCOUNTABILITY SYSTEMS (SNARE) — Public platforms, social media records, and documented commitments create an external accountability surface. The closed-door protocol mandates suppression of this surface during integration. The digital system cannot adapt or reorganize; it remains as a static, falsely-coherent record of the pre-integration self. Maximum extraction: forced maintenance of obsolete public personas while the integrator privately becomes someone else.
constraint_indexing:constraint_classification(private_identity_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THERAPEUTIC PRACTITIONER (TANGLED ROPE) — Therapists and counselors both benefit from and enforce the closed-door protocol. They benefit: privileged access to unguarded introspection creates deeper clinical data and strengthens the therapeutic alliance. They enforce: their professional role includes gatekeeping which insights are 'ready to share' and which require more integration work. Constrained exit — bound by confidentiality and clinical standards. Mixed coordination (enabling safe integration) and extraction (monopolizing access to the integrator's authentic self).
constraint_indexing:constraint_classification(private_identity_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: IDENTITY INTEGRATOR - SELF-DIRECTED (ROPE) — Individual agents pursuing autonomous identity integration see the protocol as pure coordination: temporary privacy is the mechanism that solves the collective action problem of 'how do I become a new person without external judgment destabilizing the process?' Arbitrage exit: can choose when to open the door and resume public coherence. Beneficiary — extraction flows toward this agent through controlled timing and narrative management.
constraint_indexing:constraint_classification(private_identity_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: INSTITUTIONAL THERAPY-INDUSTRIAL COMPLEX (TANGLED ROPE) — Mental health industries, coaching platforms, and identity-work providers benefit from widespread adoption of the closed-door protocol. They serve a coordination function: offering structured spaces and protocols for identity integration. They also extract: each client who internalizes the protocol becomes a repeat customer ('you're not ready to go public yet'), dependency on therapeutic gatekeeping increases, and the industry captures revenue from the integration timeline (typically 2-5+ years). Powerful institutional actors with arbitrage exit — can pivot to alternative service models — but profit incentive aligns with protocol perpetuation.
constraint_indexing:constraint_classification(private_identity_integration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PSYCHOLOGICAL AUTHENTICITY RITUAL (PITON) — The closed-door protocol is performatively legitimized by pop-psychology frames ('you need time to integrate,' 'your true self is in progress,' 'authenticity takes years'). These frames are largely theatrical — the actual verification that integration has occurred is opaque. The ritual persists through institutional inertia (therapists trained in the model, self-help industries built on it) despite weak functional justification for the 2-5 year timeline. Theater ratio (0.61) reflects that much of the protocol's enforcement is performative narrative ('you're not ready yet') rather than measured functional barriers.
constraint_indexing:constraint_classification(private_identity_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some period of private reflection may appear intrinsic to psychological development: identity formation inherently requires internal processing before external expression. This perspective naturalizes the protocol as an immutable feature of human cognition. However, the structural data contradicts the mountain classification — the closed-door mandate is a contingent institutional arrangement, not a law of psychology. Different cultures have radically different norms for identity-in-progress sharing. The engine will identify this as a false summit: naturalization of protocol.
constraint_indexing:constraint_classification(private_identity_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_identity_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_identity_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_identity_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_identity_integration, TR),
    TR >= 0.70.

:- end_tests(private_identity_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The protocol does impose real costs on excluded networks and digital systems, and therapists do exercise gatekeeping power. However, the extraction is not maximal because integrators retain exit options (they can disclose early, can switch practitioners, can work without formal therapy) and the 'private integration' model does solve a genuine coordination problem — processing identity change without external judgment and premature crystallization. The value reflects that the constraint contains both coordination and extraction elements. Suppression (0.52): Moderate-high. The closed-door mandate is enforced through social norms ('don't talk about your therapy'), professional boundaries (confidentiality requirements), and therapeutic framing ('you're not ready to share yet'). Suppression is not total because integrators can and do disclose to trusted circles, and alternative modalities (group therapy, peer support, online communities) reduce isolation. Theater ratio (0.61): Moderate-high. The protocol is substantially performative. The '2-5 year integration timeline' lacks empirical justification for why 2 years is insufficient or why 5 years is necessary. Much of the enforcement occurs through narrative gatekeeping ('you're still in process,' 'authenticity takes time') rather than measurable functional barriers. However, there is real protective function — premature public disclosure of identity-in-progress does carry genuine social risks. The theater has risen over the interval as pop-psychology has expanded the 'integration' concept to encompass increasingly subtle aspects of identity, extending the timeline and increasing the plausible space for 'not yet ready' gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the perspectival inversion at the heart of Deferential Realism. The same structural arrangement (privacy mandate during identity integration) classifies as Rope from the integrator's perspective (benefits from the constraint), Snare from the network's perspective (locked into supporting an invisible process), and Tangled Rope from the therapist's perspective (both enabling and extracting). No single classification captures the constraint's true character across all observables — the presheaf of perspectives IS the complete description. The therapy industry's Tangled Rope classification reveals that what appears as a psychological necessity (the 2-5 year timeline) may be partly a profitable institutional arrangement. The Piton classification of the authenticity ritual reveals that much of the constraint's enforcement is narrative performance rather than functional measurement. The false summit mountain classification shows the risk of naturalizing a Western institutional practice (talk therapy with privacy requirements) as a universal law of human development.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Integrators with arbitrage exit options experience low d (beneficiary position) — they control timing and benefit from the constraint's privacy protection. Excluded social networks with no exit options experience high d (victim position) — they absorb costs without compensation and cannot reorganize around the integration process. Therapists with constrained exit (bound by professional ethics) experience moderate d — they both enable coordination and extract gatekeeping authority, producing mixed directionality. The institutional therapy industry with arbitrage exit (can pivot to alternative service models) experiences low-to-moderate d — they are beneficiaries through profit but not maximal extractors because alternative models are available. The analytical observer experiences high d (analytical perspective produces maximum f(d)) — they see the full structural asymmetry and false naturalization. The piton perspective assigns d based on institutional arbitrage — the psychological ritual maintains itself through inertia, not active extraction, producing lower d than the snare perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The closed-door protocol resolves the mandatrophy by decomposing what appears as a single constraint into multiple structural claims: (1) Identity integration requires some period of private reflection (coordination problem — Rope). (2) This reflection MUST be complete isolation from social feedback for 2-5 years (extraction mechanism — Snare/Tangled Rope). (3) Therapists are uniquely equipped to determine when integration is 'complete' (gatekeeping enforcement — Tangled Rope). (4) The integration timeline is a psychological law, not a convention (false naturalization — mountain false summit). Claims 1 and 4 appear to justify claims 2-3, but they are structurally distinct. Comparative evidence from cultures with different integration norms (e.g., collectivist societies where identity is continuously negotiated in community) suggests claim 1 is true but claim 2-4 are contingent institutional artifacts. The mandatrophy is resolved by separating the genuine coordination function (claim 1: some private processing aids integration) from the extraction mechanisms (claims 2-4: indefinite timeline, professional gatekeeping, naturalized necessity). The 'tangled rope' classification at the institutional level captures this hybrid: the protocol serves a real coordination function while being leveraged for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_completion_criterion,
    'What constitutes completed identity integration? Who determines when the door can open?',
    'Empirical analysis of integration markers (behavioral consistency, narrative coherence, internal dialogue resolution); comparison across therapeutic traditions; longitudinal follow-up on agents who open the door early vs late',
    'If completion is objective/measurable: the protocol is coordination with clear endpoints (Rope/Scaffold). If completion is subjective/practitioner-determined: the protocol is extraction with indefinite gatekeeping (Snare/Tangled Rope). Current lack of criteria suggests extraction dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(integration_completion_criterion, empirical, 'Objective criteria for completed identity integration').

omega_variable(
    social_feedback_necessity,
    'Is complete isolation from social feedback actually necessary for identity integration, or does it prevent integration by denying reality-testing?',
    'Comparative study: agents with selective (trusted-circle) sharing vs complete privacy during integration; measurement of integration success rates, identity stability, and authenticity by outcome metrics; analysis of whether feedback rejection during integration predicts integration failure',
    'If isolation improves outcomes: the closed-door protocol is functionally justified (Rope/Scaffold). If selective sharing produces equal or better outcomes: the complete-privacy mandate is extractive and unnecessarily suppressive (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_feedback_necessity, empirical, 'Whether complete social isolation is necessary for successful integration').

omega_variable(
    therapeutic_incentive_alignment,
    'Do therapists'' financial and status incentives align with early door-opening or prolonged integration timelines?',
    'Analysis of therapy billing models (hourly vs outcome-based); data on average integration timelines by practitioner and financial model; comparison of integration markers to billing duration; survey of therapist attitudes toward client independence vs continued engagement',
    'If incentives favor prolonged engagement: the protocol serves extraction (Snare/Tangled Rope dominated). If incentives align with efficient outcomes: the protocol is coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_incentive_alignment, empirical, 'Alignment of therapeutic financial incentives with integration timeline').

omega_variable(
    cultural_relativism_boundary,
    'Is the 2-5 year private integration timeline a universal psychological requirement, or a Western therapeutic convention?',
    'Cross-cultural comparison of identity-formation norms; analysis of societies with different disclosure norms (collectivist vs individualist); longitudinal comparison of identity stability and authenticity outcomes across cultures with different integration protocols',
    'If universal: the protocol is Mountain-adjacent (natural constraint). If culturally contingent: the protocol is institutional extraction presented as natural law (false summit / Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_relativism_boundary, conceptual, 'Cultural universality of the closed-door integration protocol').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_identity_integration, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priv_id_tr_t0, private_identity_integration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(priv_id_tr_t2, private_identity_integration, theater_ratio, 2, 0.54).
narrative_ontology:measurement(priv_id_tr_t4, private_identity_integration, theater_ratio, 4, 0.61).

% Extraction over time
narrative_ontology:measurement(priv_id_be_t0, private_identity_integration, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(priv_id_be_t2, private_identity_integration, base_extractiveness, 2, 0.31).
narrative_ontology:measurement(priv_id_be_t4, private_identity_integration, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_identity_integration, enforcement_mechanism).
narrative_ontology:affects_constraint(private_identity_integration, authentic_self_performance).
narrative_ontology:affects_constraint(private_identity_integration, therapeutic_dependency).
narrative_ontology:affects_constraint(private_identity_integration, digital_reputation_coherence).

% DUAL FORMULATION NOTE:
% The closed-door identity protocol decomposes into three linked constraints: (1) private_identity_integration (this story) — the mandate for privacy during identity formation (ε=0.38, Tangled Rope from institutional perspective); (2) authentic_self_performance — the social demand for coherent public identity which makes private integration necessary (ε higher, upstream driver); (3) therapeutic_dependency — the professional gatekeeping of integration timeline (ε potentially higher, downstream extraction). This story focuses on the structural constraint itself; the upstream 'coherence demand' and downstream 'professional gatekeeping' are separate constraints in the family. The three stories link through network edges: authentic_self_performance → private_identity_integration → therapeutic_dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(private_identity_integration, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
