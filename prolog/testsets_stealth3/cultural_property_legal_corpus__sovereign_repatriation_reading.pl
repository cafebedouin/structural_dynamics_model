% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Regime for Colonial-Era Cultural Property
 *   domain: international law/cultural property/post-colonial studies
 *
 * SUMMARY:
 *   A normative-legal regime in which post-colonial successor states assert
 *   sovereign ownership of cultural property removed during imperial rule,
 *   treating colonial-era acquisition as lacking valid title and themselves
 *   as legitimate heirs of the expropriated polities. The regime operates
 *   almost entirely through soft law and bilateral diplomacy: claimant states
 *   file documented claims, encyclopedic museums in former imperial capitals
 *   answer with provenance research, retention arguments, loans, or
 *   negotiated returns, intergovernmental bodies codify norms and host
 *   mediation, and national statutes bar many holders from disposing of
 *   collection items outright. Enforcement is reputational and diplomatic
 *   rather than judicial, and its machinery has been built up continuously
 *   since the 1950s. Base extractiveness is authored for the regime's
 *   operation as it actually stands (the standing contested arrangement this
 *   story is about), not for an idealized fully-executed restitution
 *   end-state. KEY AGENTS (by structural relationship): -
 *   successor_state_governments: primary beneficiary
 *   (institutional/constrained) - files claims, receives restituted
 *   patrimony, converts returns into legitimacy capital, co-sets the agenda -
 *   foreign_holding_institutions: primary target
 *   (institutional/identity_locked) - bears disgorgement pressure and
 *   reputational cost; legally barred from disposal and unable to abandon
 *   their universal-collection self-conception -
 *   descendant_communities_expropriated_peoples: vindicated-in-name party
 *   (moderate/constrained) - patrimony restored in their name but delivered
 *   through state custody - successor_state_publics: secondary beneficiary
 *   (moderate/constrained) - holder_country_museum_publics: diffuse
 *   cost-bearers (moderate/mobile) - access losses offset by institutional
 *   ethical renewal - unesco_icom_softlaw_bodies: procedural administrator
 *   (institutional/constrained) - transnational_art_market_intermediaries:
 *   excluded affected party (powerful/arbitrage)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.54).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Regime for Colonial-Era Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international law/cultural property/post-colonial studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'b2497851-a569-4270-bace-90500a5f4198').
narrative_ontology:cs_kernel_codification('b2497851-a569-4270-bace-90500a5f4198', distributed).
narrative_ontology:cs_authority_grounding('b2497851-a569-4270-bace-90500a5f4198', distributed).
narrative_ontology:cs_reading_relation('b2497851-a569-4270-bace-90500a5f4198', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2497851-a569-4270-bace-90500a5f4198', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('b2497851-a569-4270-bace-90500a5f4198', foundational, colonial_acquisition_void_of_valid_title).
narrative_ontology:cs_axiom_status(colonial_acquisition_void_of_valid_title, holdable).
narrative_ontology:cs_axiom_grounding('b2497851-a569-4270-bace-90500a5f4198', colonial_acquisition_void_of_valid_title, deontological).
narrative_ontology:cs_axiom('b2497851-a569-4270-bace-90500a5f4198', foundational, historical_continuity_confers_sovereign_authority).
narrative_ontology:cs_axiom_status(historical_continuity_confers_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('b2497851-a569-4270-bace-90500a5f4198', historical_continuity_confers_sovereign_authority, conventional).
narrative_ontology:cs_reference_frame('b2497851-a569-4270-bace-90500a5f4198', successor_state_patrimonial_sovereignty).
narrative_ontology:cs_drift_state('b2497851-a569-4270-bace-90500a5f4198', contemporary_restitution_wave, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b2497851-a569-4270-bace-90500a5f4198', '2026-07-28T09:15:00Z').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_publics).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, descendant_communities_expropriated_peoples).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, foreign_holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holder_country_museum_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, holder_country_museum_publics).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, transnational_art_market_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Post-colonial national governments that assert ownership of cultural property removed during imperial rule within their territories. They file restitution claims through diplomatic channels, negotiate bilateral return agreements, receive restituted objects into national museums under cultural ministries, and cite recovered patrimony in domestic legitimacy narratives. They depend on holder cooperation and on the soft-law framework for leverage; unilateral seizure would cost them international standing, so they work through the negotiated channel they also help set the agenda for.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, agenda_setter).

% Encyclopedic museums, university collections, and royal collections in former imperial capitals holding large colonial-era acquisitions. Several operate under national statutes that bar disposing of collection items, and their governing self-conception is built on holding representative world collections in one place under one roof. They respond to restitution claims with provenance research, retention arguments, long-term loans, or negotiated returns; wholesale relinquishment would unravel founding collections and the institutional self-image bound to them, so shedding contested holdings is not a real option from where they stand.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, foreign_holding_institutions, payer,
    institutional, generational, identity_locked, global).

% The peoples and polities from whom objects were originally taken: royal courts, religious communities, lineages, and their successors. Restitution claims are filed in their name, but the successor state is the recognized interlocutor and the designated receiving custodian, so returned objects typically enter national institutions rather than community keeping places. Some communities negotiate directly with individual museums alongside state channels; most depend on state mediation for standing, for funding, and for where returns ultimately land.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, descendant_communities_expropriated_peoples, beneficiary,
    moderate, generational, constrained, continental).

% Citizens of claimant states, for whom returned patrimony is a visible national achievement and, prospectively, a domestic cultural and tourist asset. Their access to restored objects depends on national institutions displaying them; they have little direct influence over which claims their governments prioritize or how returns are staged.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_publics, beneficiary,
    moderate, generational, constrained, national).

% Museum-going publics in former imperial countries who have historically accessed world collections close to home. Each restitution reduces what they can see locally, while renewing the ethical standing of the institutions they fund, visit, and identify with; they experience the regime chiefly as changing gallery contents and recurring public controversy, and they can redirect attention and leisure spending elsewhere at little personal cost.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holder_country_museum_publics, payer,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, holder_country_museum_publics, beneficiary).

% Intergovernmental and professional bodies: the UNESCO intergovernmental committee and 1970 Convention framework, ICOM code and red-list processes. They codify restitution norms, mediate specific disputes, publish endangered-category lists, and provide the procedural venue through which claims run. Their authority is convening and standard-setting; they command no enforcement force beyond member-state cooperation and are bound by the consensus of the same member states they press.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_icom_softlaw_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Auction houses, dealers, freeport storage operators, and private collectors holding or trading colonial-era material. Due-diligence norms and provenance scrutiny compress the market for unprovenanced pieces, and import/export regulations raise transaction costs. They are rarely seated in restitution negotiations yet absorb compliance costs and control significant unstudied material; cross-jurisdictional storage and privacy structures allow them to relocate assets ahead of scrutiny.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, transnational_art_market_intermediaries, excluded,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, transnational_art_market_intermediaries, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts scattered, potentially explosive acquisition disputes into a standardized claim-negotiation-return procedure: a shared vocabulary of provenance and documented claim criteria, diplomatic protocol, and predictable institutional responses. Solves the collective-action problem that unfettered bilateral improvisation would otherwise produce: ad hoc seizures, frozen cultural relations, and a legal free-for-all over every contested object.
% TRANSFER_FUNCTION: Moves physical custody of colonial-era objects, and the symbolic capital attached to them, from foreign holding institutions to successor-state national custody. Moves diplomatic leverage and moral-authority standing toward claimant states. Moves conservation, insurance, transport, and reputational costs onto returning institutions and, diffusely, onto holder-country audiences.
% ABSENT_VOICES: Transnational art-market intermediaries control much colonial-era material but are structurally outside the negotiation venues. Descendant communities seeking direct custodial authority participate mainly through state mediation. Cosmopolitan publics committed to universal access have no formal seat and are routinely reframed as apologists for retention. Diaspora descendants outside recognized state succession lines have no standing at all.
% DISAPPEARANCE_RATIONALE: If the sovereign-restitution framework vanished overnight, dozens of active bilateral negotiations would freeze, the current wave of state-led returns would stall for lack of a legitimating script, museums would revert to purely legal-possession defenses, and claimant states would lose their principal instrument for converting colonial grievance into actionable diplomacy; the field would reorganize around litigation and market forces with no shared procedure.
% FOUNDING_PROBLEM: The post-decolonization legitimacy gap: newly independent states inherited borders and identities while the pre-colonial heritage that could anchor those identities sat in imperial capitals, and thousands of colonial-era acquisitions existed with no agreed procedure for contesting them. The framework was built to convert historical dispossession into structured, actionable sovereign claims.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the French government's commissioned Sarr-Savoy report (a holder-state seat) concluded colonial takings lacked valid title; the German federal government acknowledged wrongful acquisition in the Benin returns process (holder-state seat); UNESCO intergovernmental committee records document continuing unresolved claims; and holder-country academic provenance research (e.g., Pitt Rivers and Oxford scholarship on the Benin raid) attests the illegitimate character of the original acquisitions. None of these corroborating seats collects restitution gains.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed_type tangled_rope states what I believe is structurally true: the regime has a genuine coordination function (a standardized claim-negotiation-return channel replacing chaotic improvised seizure disputes) AND asymmetric extraction (holders disgorge contested objects and bear costs while successor-state governments collect the symbolic capital), held together by active enforcement (soft-law codification, conditioned loans, national statutes, reputational pressure). The metrics describe the regime's actual operation without tuning toward that claim.
 *   
 *   Extractiveness 0.54: real disgorgement, compliance, and access costs fall on identifiable seats, offset by restored symbolic capital and a functioning dispute channel; neither negligible nor predatory. Suppression 0.42: coercion is reputational and statutory rather than physical, and alternatives (long-term loans, joint custody, digital access, community-side agreements) remain visibly available, so alternatives have not collapsed (accessibility_collapse 0.31). Resistance 0.62: holder institutions and universal-access constituencies contest the regime actively and continuously.
 *   
 *   Temporal series run on one shared seven-point grid (t0=1954 Hague Convention baseline, unit approximately one year, tn=present) with all three metrics authored at every point and endpoints matching the scalar values. The suppression_requirement series is authored deliberately: the story's dynamic is enforcement-capacity build-out (from a legal vacuum, through the 1970 Convention and red-list machinery, to normalized state-led returns), so enforcement maturation is the traced quantity, not merely shifting extraction. Theater_ratio rises as headline symbolic returns and institutional virtue-display outpace structural change; genuine returns coexist with a growing performative share. The series is monotonic, with no oscillatory cycle, so no cyclical apparatus is invoked. Coordination type identity_coordination is declared because the regime's dominant function is adjudicating identity-grounded entitlement claims; the known gaming risk of identity framing is carried explicitly in the state_capture_of_community_gain omega.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from structural data. From the successor-state seat the regime is a justice-restoring coordination instrument it co-authors; from the holder seat the identical structure operates as mounting enforced disgorgement it cannot escape; from the descendant-community seat restitution arrives mediated, with custody decisions made in distant capitals; from the market seat it is an external compliance cost absorbed from outside the room.
 *   
 *   Identity-lock dynamics for foreign_holding_institutions: the binding mechanism is institutional identity fusion. The universal-museum project (formalized in the 2002 universal-museums declaration) fused the institutions' self-concept to holding representative world collections in one place; several are additionally statute-barred from disposal, so the identity layer sits atop a legal trap. Deaccession is experienced not as a transaction but as self-negation, which is why exit_options is identity_locked rather than merely constrained. If the identity frame broke, the computed profile would shift materially: suppression as experienced would drop sharply, exit would resolve toward constrained-or-mobile, and these seats could pivot from retention defense to leadership of negotiated return, converting their seat's classification from target toward coordinated participant.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. successor_state_governments sit near the full-beneficiary end: they collect returns and symbolic capital while paying only diplomatic friction. successor_state_publics sit near them, gaining restored patrimony and identity dividends at second hand. foreign_holding_institutions sit near the full-target end: the victim declaration combined with identity_locked exit pushes their effective extraction toward the maximum the scaling permits, since trapped-or-locked targets amplify. holder_country_museum_publics sit target-side but attenuated: their costs are diffuse access losses and their mobility dampens the bite. descendant_communities_expropriated_peoples derive a low directionality from the beneficiary declaration, but their realized gain is discounted by state mediation; that intra-class nuance is routed to the state_capture omega rather than a directionality_override, because overrides key on the power atom and the moderate atom is shared with two other seats whose derived directionality is already correct - a blanket override would corrupt the neighboring seats. No overrides are declared: the derivation produces the right d at the class level for every seat. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness alone is scaled, by directionality and spatial scope, in the engine's computation, which is why the global scope of the claim network modestly amplifies the holder seats' effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The regime's founding problem is live and externally corroborated (holder-state acknowledgments, intergovernmental records, holder-country scholarship), so there is no mandatrophy to resolve and the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges as coherent rather than as a zombie flag. The forward risk is the opposite degradation: if the major claims are eventually satisfied, the regime's coordination function could atrophy while provenage rituals and symbolic-return ceremonies continue, drifting toward theatrical maintenance of a completed mission. The theater_ratio series (0.15 to 0.43 and climbing) is the tripwire for that drift: it is rising but functional activity still dominates. The tangled_rope claim keeps both truths legible - the regime cannot be dismissed as pure extraction (the coordination channel is real and load-bearing) nor celebrated as pure restitution (disgorgement costs are real and asymmetrically borne).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading (sovereign_repatriation_reading) of the contested kernel cultural_property_legal_corpus; which reading''s constraint ought to govern disposition of colonial-era cultural property?',
    'Comparative evaluation across the three sibling constraint files against observed restitution outcomes; no adjudication inside this file. This file authors only the sovereign reading as a clean, epsilon-invariant constraint.',
    'Switching readings replaces the entire beneficiary set (successor states, versus descendant communities, versus humanity-wide preserving institutions), relocates epsilon, and changes enforcement form from state diplomacy to institutional ethics codes or community custodianship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame position: this file instantiates one of three rival readings of the cultural-property kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    authority_locus_disagreement_point,
    'Where do the sibling readings structurally diverge, and what would adopting a sibling change? The located disagreement is the locus of legitimate authority over colonial-era objects: a state claiming historical continuity with the expropriated polity, versus a community maintaining cultural continuity, versus whichever institution maximizes preservation and access.',
    'Trace which seat each real restitution outcome empowers: objects landing under national ministries confirm this reading''s structure; objects placed under community custody confirm the indigenous-stewardship structure; durable loan-sharing and shared-stewardship regimes confirm the universal-heritage structure.',
    'Adopting the indigenous-stewardship reading converts successor states from beneficiaries into intermediaries or obstacles and redirects disgorgement toward communities; adopting the universal-heritage reading converts this reading''s payee institutions into the legitimate authority and recasts restitution claims as access restrictions on humanity''s heritage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_locus_disagreement_point, conceptual, 'Sibling structural delta and the specific element (authority locus) on which the readings disagree.').

omega_variable(
    state_capture_of_community_gain,
    'Does restitution delivered through successor-state channels actually reach descendant communities, or is it captured by national institutions and state elites?',
    'Track post-return custody chains: the share of restituted objects physically accessible to originating communities versus sequestered in capital-city national stores; the presence and weight of community consultation in signed return agreements.',
    'If captured, this reading''s coordination function degrades toward elite symbolic-capital accumulation, and the burden of the regime redistributes onto the very communities it names as vindicated; the community seat''s computed classification would shift accordingly. This is also the identity-coordination gaming watchpoint: extraction concentrating on less powerful agents at national scope signals nonsensical coupling behind the identity frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_of_community_gain, empirical, 'Whether state mediation delivers or intercepts the restitution gains promised to descendant peoples.').

omega_variable(
    continuity_claim_boundary,
    'Which states validly count as ''claiming historical continuity with expropriated peoples''? The criterion bounding this reading''s beneficiary set is itself underdetermined: multi-ethnic successor states inherit several expropriated polities at once, settler-founded states advance patrimony claims, and succession lines are politically constructed.',
    'Comparative jurisprudence on how national and international instruments operationalize continuity (territorial succession versus ethnographic descent versus polity inheritance), plus case-by-case adjudication records of accepted and rejected continuity claims.',
    'A restrictive criterion narrows the beneficiary set and concentrates per-claimant gains; a permissive one dilutes the claim class and lets loosely-related states ride restitution momentum, moving measured extraction in opposite directions depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_boundary, conceptual, 'Boundary underdetermination of the reading''s own legitimacy criterion, which fixes which claims the regime recognizes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_repatriation_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sov_repatriation_tr_t12, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(sov_repatriation_tr_t24, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(sov_repatriation_tr_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(sov_repatriation_tr_t48, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(sov_repatriation_tr_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(sov_repatriation_tr_t72, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 72, 0.43).

% Extraction over time
narrative_ontology:measurement(sov_repatriation_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sov_repatriation_be_t12, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(sov_repatriation_be_t24, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(sov_repatriation_be_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 36, 0.44).
narrative_ontology:measurement(sov_repatriation_be_t48, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 48, 0.49).
narrative_ontology:measurement(sov_repatriation_be_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(sov_repatriation_be_t72, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 72, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(sov_repatriation_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(sov_repatriation_su_t12, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(sov_repatriation_su_t24, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 24, 0.17).
narrative_ontology:measurement(sov_repatriation_su_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 36, 0.23).
narrative_ontology:measurement(sov_repatriation_su_t48, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 48, 0.29).
narrative_ontology:measurement(sov_repatriation_su_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(sov_repatriation_su_t72, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 72, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, identity_coordination).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who owns colonial-era cultural property' decomposes into three structurally distinct constraints (one per reading of the cultural_property_legal_corpus kernel), each with its own epsilon, beneficiary set, and failure modes. This file instantiates the sovereign-repatriation reading: successor states as beneficiaries, holders as payees, moderate extraction from repatriation costs and diplomatic friction offset by restored symbolic capital. The universal-heritage sibling authors epsilon around access restriction and preservation risk; the indigenous-stewardship sibling authors epsilon around community dispossession by both museums and successor states. The readings are linked as a family via affects_constraints because each one's operation changes the legitimacy conditions and custody destinations available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
