% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Conditional Free Movement under Member-State Consent (Sovereignty-Primary Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the
 *   federation_membership_treaty kernel: free movement within the federation
 *   is a conditional grant, bounded and revocable by member-state consent,
 *   with states retaining authority to protect national labor markets and
 *   welfare systems. The referent of epsilon is the standing arrangement
 *   under contest — the conditional-movement regime itself — assessed by this
 *   reading's own lights: the reading deems the protection functions
 *   legitimate democratic governance while acknowledging that mobile workers
 *   from other member states bear restricted access as a real cost. The
 *   sibling readings (integration_primary, subsidiarity_balance) instantiate
 *   different constraints from the same treaty text and are authored as
 *   separate files; this story does not average over them. The claim
 *   (tangled_rope) and the metrics are authored independently: this reading
 *   acknowledges both a genuine coordination function (managing labor flows
 *   across distinct, contributory welfare systems) and asymmetric extraction
 *   (mobile workers pay through the same structure that coordinates and
 *   protects domestic constituencies). KEY AGENTS (by structural
 *   relationship): member_state_governments: agenda-setter and principal
 *   beneficiary (institutional/mobile) — administers the eligibility and
 *   safeguard machinery, collects electoral credit from protection politics;
 *   sheltered_sector_workers: beneficiary (organized/constrained) — domestic
 *   labor shielded from cross-border wage competition;
 *   welfare_state_contributors: beneficiary (moderate/constrained) — existing
 *   contributors whose claimant pools are fenced;
 *   mobile_workers_from_other_member_states: primary target
 *   (moderate/constrained) — bear conditional access, eligibility testing,
 *   and removal exposure; cross_border_employers: secondary target
 *   (powerful/constrained) — recruitment friction and compliance costs, with
 *   lobbying leverage the workers lack; supranational_court: analytical
 *   observer (institutional/analytical) — adjudicates mobility disputes,
 *   narrowing state discretion; third_country_nationals: excluded
 *   (powerless/trapped) — outside the regime entirely.
 *
 * KEY AGENTS:
 *   - member_state_governments: agenda-setter and principal beneficiary (institutional/mobile) — administers eligibility and safeguard machinery, collects electoral credit, holds treaty-level exit no other seat has
 *   - sheltered_sector_workers: beneficiary (organized/constrained) — domestic labor protected from cross-border competition in shielded sectors
 *   - welfare_state_contributors: beneficiary (moderate/constrained) — existing contributors whose benefit pools are fenced against non-contributing entrants
 *   - mobile_workers_from_other_member_states: primary target (moderate/constrained) — bear conditional access and removal exposure; dispersed as a class and unable to vote where they work
 *   - cross_border_employers: secondary target (powerful/constrained) — recruitment friction and compliance costs; lobby and litigate for liberalization
 *   - supranational_court: analytical observer (institutional/analytical) — its jurisprudence is the main channel of sibling-reading pressure into this arrangement
 *   - third_country_nationals: excluded (powerless/trapped) — would object to the boundary's placement but hold no seat in any deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.55).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Conditional Free Movement under Member-State Consent (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '2d66a4e1-dd5b-4736-a0bd-89fd644756e9').
narrative_ontology:cs_kernel_codification('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', fixed_text).
narrative_ontology:cs_authority_grounding('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', lineage).
narrative_ontology:cs_interpretation_layer_present('2d66a4e1-dd5b-4736-a0bd-89fd644756e9').
narrative_ontology:cs_reading_relation('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', federation_membership_treaty__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', foundational, member_state_consent_conditions_mobility).
narrative_ontology:cs_axiom_status(member_state_consent_conditions_mobility, holdable).
narrative_ontology:cs_axiom_grounding('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', member_state_consent_conditions_mobility, conventional).
narrative_ontology:cs_axiom('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', foundational, democratic_communities_define_solidarity_boundaries).
narrative_ontology:cs_axiom_status(democratic_communities_define_solidarity_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', democratic_communities_define_solidarity_boundaries, deontological).
narrative_ontology:cs_reference_frame('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', member_states_as_treaty_masters).
narrative_ontology:cs_drift_state('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', contemporary_citizenship_jurisprudence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2d66a4e1-dd5b-4736-a0bd-89fd644756e9', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, sheltered_sector_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_state_contributors).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers_from_other_member_states).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_employers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, member_state_consent_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, welfare_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the conditions of access: safeguard clauses, residence registration, benefit eligibility tests, and removal powers. Collect electoral credit from protection politics and retain treaty-level exit — renegotiation, opt-outs, withdrawal — that no other seat holds. They fund the enforcement machinery and bear the litigation the supranational court brings against restrictive practice.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, mobile, national).

% Work in sectors and regions shielded from cross-border labor competition by the consent conditions. Their wages and job queues are protected by the restriction machinery. Their exit is limited: skills, housing, and family ties bind them to the domestic market they are protected within.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, sheltered_sector_workers, beneficiary,
    organized, biographical, constrained, national).

% Pay contributions into national systems whose claimant pools the consent conditions fence against non-contributing entrants. They receive fiscal shielding without administering anything. Their exposure to the arrangement is indirect — through contribution levels and benefit adequacy.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, welfare_state_contributors, beneficiary,
    moderate, biographical, constrained, national).

% Seek employment and residence in member states other than their own. Access is conditional: registration, eligibility tests, and removal exposure attach to their status, and benefit rights are bounded by contribution and residence rules they did not set. They can move among states that keep access open and can return home, but no destination within the federation frees them from the conditionality structure itself. As a class they are dispersed across jurisdictions and cannot vote in the polities that restrict them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers_from_other_member_states, payer,
    moderate, biographical, constrained, continental).

% Operate across member states and want access to cross-border labor. The consent conditions impose recruitment friction, eligibility compliance costs, and planning uncertainty. They lobby for liberalization and litigate through industry associations; exiting the federation's market is not a realistic option for them.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_employers, payer,
    powerful, biographical, constrained, continental).

% Adjudicates disputes over the mobility rules' interpretation and the validity of national restrictions. Its jurisprudence has narrowed state discretion over the years. It collects nothing and pays nothing; its seat is analytical, and its rulings are the main channel through which the sibling readings' pressure enters this arrangement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, supranational_court, observer,
    institutional, generational, analytical, continental).

% Reside in or seek entry to the federation from outside it. The mobility regime's boundary excludes them entirely — they hold none of the conditional rights that member-state nationals carry. They would object to the boundary's placement but have no seat in any member state's or the federation's deliberations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor allocation across a federation of states that retain distinct, contributory welfare systems: the consent mechanism lets states manage inflows while the treaty framework keeps the internal market and mutual recognition operating. It also coordinates the boundary of each national welfare community — who may claim — which unconstrained market integration would dissolve.
% TRANSFER_FUNCTION: Moves regulatory authority over labor market access and welfare eligibility from the federation level down to member states, and moves the costs of that reservation — restricted access, eligibility testing, removal exposure — onto mobile workers from other member states and onto employers seeking cross-border labor.
% ABSENT_VOICES: Mobile workers bear the restrictions but vote in their home states, not in the member states that restrict them — the representation gap of conditional membership. Third-country nationals are outside the regime entirely and hold no seat anywhere in the system. Both would object if present: the workers to the conditionality itself, the third-country nationals to the boundary that defines the group they are excluded from.
% DISAPPEARANCE_RATIONALE: If the consent conditionality vanished overnight, labor flows would reorganize within months, sheltered-sector wages would face immediate cross-border competition, and each welfare system would face claimant pools it can no longer bound — forcing either federalized eligibility rules or explicit national exit from the mobility regime. The arrangement is load-bearing for the current division of welfare authority.
% FOUNDING_PROBLEM: The treaty founders had to pool enough economic sovereignty to build a common market while every state kept a distinct, contributory welfare system funded by its own nationals — the problem of managing cross-border labor without dissolving the national solidarity boundaries the welfare systems rest on.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the supranational court's docket and the Commission's enforcement actions attest the tension is live (they litigate it from the integrationist side), and the federalism and migration-economics literature documents it as unresolved. Migrant-worker advocacy organizations attest the cost side. No party disputes that the tension exists; the readings dispute its resolution — which is why the status is live rather than dead.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end): this reading's own lights acknowledge that conditional access imposes real costs on mobile workers while deeming the protection functions largely genuine — hence the tangled_rope claim rather than rope. Suppression (0.55) is authored as a raw structural property, unscaled by power or scope: the regime is held up by active enforcement machinery (registration, eligibility tests, removal powers) and by political containment of the unconditional-movement alternative, which remains live — accessibility_collapse is 0.40 because the sibling readings persist in courts and politics. Resistance (0.60) is constant: litigation before the supranational court, employer lobbying, periodic political crisis. Theater (0.33) reflects a protection rationale that is real in sheltered sectors but whose welfare leg runs ahead of the fiscal-incidence evidence. The measurement series run on one shared time grid — all three metrics at all seven points, with base_properties matching the end-of-interval state. The trajectory is a ratchet, not a cycle: enforcement capacity and extractiveness step up around enlargement and fiscal-crisis events and do not relax between them. Coordination type is identity_coordination because the function whose failure would most directly dissolve the arrangement is maintenance of the national welfare-community boundary; the FNL alert applies — the identity framing is partly cover, which the welfare_tourism_empirics omega measures directly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the member-state government seat the arrangement is self-government: the same structure that restricts mobile workers constitutes the democratic authority to define the welfare community, and with mobile exit (renegotiation, opt-outs, withdrawal) that seat computes near the coordination end. From the mobile-worker seat the same structure operates as conditional exclusion enforced by eligibility machinery — constrained exit and no vote in the restricting polity push that seat toward the extraction end. Sheltered workers and welfare contributors experience genuine protection; cross-border employers bear friction but retain lobbying and litigation leverage the mobile workers lack. Coalition potential for the dispersed victim class exists in principle (transnational unions, mobile-worker associations) but is structurally weak: the class is spread across jurisdictions, disenfranchised where the restrictions bind, and unable to concentrate its vote anywhere the constraint is set. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end of d: member_state_governments collect autonomy and electoral credit and hold arbitrage-grade exit via treaty renegotiation; sheltered_sector_workers collect wage protection; welfare_state_contributors collect fiscal shielding. Victims sit near the full-target end: mobile_workers_from_other_member_states bear the transfer — restricted access, eligibility testing, removal exposure — with constrained exit (they can move among open states and return home, but no destination within the federation frees them from the conditionality structure itself); cross_border_employers bear recruitment friction with even less exit, though their power dampens the burden relative to the workers. The supranational_court holds an analytical seat with neutral directionality. Third-country nationals are excluded rather than coordinated — their exclusion is the boundary the constraint maintains, and their absence from the conversation is itself a structural fact the absent_voices answer records.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a pooled labor market with distinct national welfare solidarities — is live, so this is not a mandatrophy case and mandatrophy_resolved is not declared. The tangled_rope classification prevents two mislabels. Reading the arrangement as pure rope would erase the mobile worker's seat: the extraction is real, asymmetric, and enforced by machinery the states control. Reading it as pure snare would erase the genuine coordination function: a federation of distinct, contributory welfare systems does need a consent mechanism over access, and the protection functions are real in sheltered sectors. The classification holds both — coordination that extracts — with the extraction's justification (democratic authority over solidarity boundaries) contested by the sibling readings rather than dissolved by this one. The rising theater_ratio series is the early-warning channel: if the welfare-protection leg continues to decouple from the fiscal evidence while enforcement machinery keeps hardening, the arrangement drifts from coordination-with-extraction toward extraction-with-coordination-cover, and the omega battery (welfare_tourism_empirics, consent_vs_electoral_capture) is what would register it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_reading_contest,
    'This constraint is one reading of the federation_membership_treaty kernel — the sovereignty_primary reading, under which mobility is a conditional grant revocable by member-state consent. Would the integration_primary sibling reading restructure the constraint''s victim set and epsilon?',
    'Treaty revision, the trajectory of supranational court doctrine on mobility and citizenship rights, or an explicit political settlement among member states on the treaty''s default.',
    'Under integration_primary, restricting states become presumptive violators rather than rights-holders, mobile workers become rights-holders rather than conditional entrants, and epsilon for restriction regimes rises sharply; this reading''s protection functions would be re-read as unjustified barriers. The disagreement is located in the treaty''s default: constitutive right versus conditional grant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_reading_contest, conceptual, 'Which reading of the treaty governs — constitutive mobility right versus member-state conditional grant.').

omega_variable(
    welfare_tourism_empirics,
    'Do mobile workers impose net fiscal costs on host welfare systems, or is the welfare-protection rationale largely performative?',
    'Fiscal-incidence studies comparing mobile workers'' contributions and benefit claims against native cohorts across member states and system types.',
    'If mobile workers are net contributors, the theater component rises, the welfare leg of the coordination story thins, and the constraint drifts toward extraction defended by rhetoric; if net costs are real in specific systems, the protection function is genuine there and the coordination claim strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_tourism_empirics, empirical, 'Empirical basis of the welfare-protection justification.').

omega_variable(
    consent_vs_electoral_capture,
    'Is member-state restriction driven by measured labor-market protection needs, or by electoral capture — governments collecting political rents from restriction politics?',
    'Compare restriction patterns (safeguard invocations, eligibility tightenings) against measured labor-market impacts across sectors and regions; look for restrictions concentrated where electoral payoff is high but measured labor impact is low.',
    'If capture dominates, the agenda_setter seat''s directionality rises (governments extract political rents through the constraint), pushing per-seat classifications toward snare and raising the effective extraction the mobile-worker seat experiences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_electoral_capture, empirical, 'Genuine labor-market protection versus political rent collection in restriction decisions.').

omega_variable(
    deterrence_internalization,
    'Is the restricted-access burden on mobile workers carried by formal legal barriers alone, or also by internalized deterrence — anticipated discrimination self-excluding workers who are formally eligible?',
    'Mobility-flow response to formal liberalizations: if flows remain below formal-eligibility predictions after rules relax, internalized deterrence carries a measurable share of the burden.',
    'If internalized deterrence dominates, formal liberalization overstates the freedom gained — effective suppression persists after barrier removal and the constraint''s true suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_internalization, empirical, 'Structural versus internalized share of the access burden on mobile workers.').

omega_variable(
    authority_grounding_framing,
    'Does this reading''s authority structure rest on the founding compact among sovereign states (lineage), or on the distributed practice of the institutions that administer the treaty — a framing under which this reading''s reference frame itself becomes contested?',
    'Examine which source is operative when access rules change in practice — whose consent moves first, whose acquiescence is required, and whether institutional practice ever outruns formal state consent without reversal.',
    'Under the distributed-practice framing, the drift vector shifts from authority_erosion toward practice_drift, and this reading''s foreclosure of integration_primary weakens, since a practice-based framework can absorb both readings without contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Alternative framing of the authority structure beneath the sovereignty reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t0, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t5, federation_membership_treaty__sovereignty_primary, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t5, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t10, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t15, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t20, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t25, federation_membership_treaty__sovereignty_primary, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t25, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t0, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t5, federation_membership_treaty__sovereignty_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t5, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t10, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t15, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t20, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t25, federation_membership_treaty__sovereignty_primary, base_extractiveness, 25, 0.54).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t25, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t0, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t5, federation_membership_treaty__sovereignty_primary, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t5, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t10, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t15, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t20, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t25, federation_membership_treaty__sovereignty_primary, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t25, observed).
narrative_ontology:measurement(fed_treaty_sovereignty_primary_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(fed_treaty_sovereignty_primary_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Constraint family: the federation_membership_treaty kernel decomposes into three readings — integration_primary (mobility constitutive), sovereignty_primary (this file; mobility conditional on member-state consent), and subsidiarity_balance (proportionality-bounded mobility right). Each reading instantiates a different constraint over the same treaty text with its own epsilon, beneficiary/victim structure, and classification; they are linked rather than merged because one story cannot hold a single stable epsilon across readings (epsilon-invariance). The sovereignty reading is downstream of the integration reading in discourse: integrationist jurisprudence supplies the pressure that erodes this reading's reference frame, which is why the edge runs toward the integration sibling as well.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
