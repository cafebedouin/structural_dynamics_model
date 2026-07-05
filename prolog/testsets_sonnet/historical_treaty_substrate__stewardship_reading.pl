% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Stewardship Reading (Relational Pact for Shared Territorial Stewardship)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the historical_treaty_substrate
 *   kernel — the stewardship reading, under which the original treaties did
 *   not transfer sovereignty but established a relational, ongoing pact for
 *   shared territorial stewardship between Indigenous nations and the settler
 *   state. Under this reading, resource governance should be joint,
 *   consent-based, and continuously renegotiated rather than a completed
 *   transaction (the extinguishment_reading) or a fully symmetric
 *   international relationship (the nation_to_nation_reading). The constraint
 *   modeled here is the GAP between what the stewardship reading commits the
 *   settler state to and what actually happens: administrative practice
 *   largely proceeds as though extinguishment occurred, while courts and
 *   treaty bodies increasingly cite stewardship-reading language to compel
 *   partial co-management. This produces a tangled rope: genuine coordination
 *   function (shared occupation of one territory by two polities without
 *   perpetual conflict) coexists with asymmetric extraction (resource revenue
 *   and unilateral administrative control accrue to the settler state and its
 *   licensees, while Indigenous nations bear enforcement costs, degraded
 *   territory, and unimplemented commitments).
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: primary rights-holder under this reading, bears extraction while nominally structural beneficiary of the reading's jurisdictional claim
 *   - settler_state_resource_sector: extracts territorial resources under licenses that presuppose extinguishment, benefiting from the gap between the reading and administrative practice
 *   - settler_state_government: agenda-setter administering treaty implementation, whose compliance with stewardship obligations is the central contested variable
 *   - future_generations_of_treaty_territory: powerless payer bearing ecological and jurisdictional costs of present extraction decisions
 *   - treaty_and_land_rights_tribunals: analytical observer whose interpretive choice among kernel readings determines enforceability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.61).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.72).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Stewardship Reading (Relational Pact for Shared Territorial Stewardship)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'ba754f3f-a4df-447f-9420-bc1340e4953c').
narrative_ontology:cs_kernel_codification('ba754f3f-a4df-447f-9420-bc1340e4953c', fixed_text).
narrative_ontology:cs_authority_grounding('ba754f3f-a4df-447f-9420-bc1340e4953c', lineage).
narrative_ontology:cs_interpretation_layer_present('ba754f3f-a4df-447f-9420-bc1340e4953c').
narrative_ontology:cs_reading_relation('ba754f3f-a4df-447f-9420-bc1340e4953c', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('ba754f3f-a4df-447f-9420-bc1340e4953c', historical_treaty_substrate__nation_to_nation_reading, influences).
narrative_ontology:cs_axiom('ba754f3f-a4df-447f-9420-bc1340e4953c', foundational, no_sovereignty_cession_occurred).
narrative_ontology:cs_axiom_status(no_sovereignty_cession_occurred, holdable).
narrative_ontology:cs_axiom_grounding('ba754f3f-a4df-447f-9420-bc1340e4953c', no_sovereignty_cession_occurred, empirically_contingent).
narrative_ontology:cs_axiom('ba754f3f-a4df-447f-9420-bc1340e4953c', foundational, mutual_stewardship_obligation_is_continuous).
narrative_ontology:cs_axiom_status(mutual_stewardship_obligation_is_continuous, holdable).
narrative_ontology:cs_axiom_grounding('ba754f3f-a4df-447f-9420-bc1340e4953c', mutual_stewardship_obligation_is_continuous, deontological).
narrative_ontology:cs_reference_frame('ba754f3f-a4df-447f-9420-bc1340e4953c', relational_coexistence_pact).
narrative_ontology:cs_drift_state('ba754f3f-a4df-447f-9420-bc1340e4953c', contemporary_resource_development_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ba754f3f-a4df-447f-9420-bc1340e4953c', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_territory).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, coexistence_as_founding_intent).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, territorial_stewardship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the original treaty relationship as the basis of ongoing jurisdiction over shared territory and its resources under the stewardship reading — the land was never surrendered, only its use shared. In practice they receive fractional recognition of this jurisdiction: consultation processes, limited co-management boards, and litigation wins that are often under-implemented. They cannot exit the relationship (their territory and identity are bound to it) and depend on courts and treaty bodies to enforce obligations the settler state resists honoring in full.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, payer).

% Extracts resources (timber, minerals, hydro capacity, fisheries) from treaty territory under licenses issued by the settler state, often without the joint-management consent the stewardship reading requires. Benefits from the state's practical administration of the land as though ceded, while citing treaty language selectively when convenient. Can relocate capital or shift extraction elsewhere if regulatory friction increases — an exit option the Indigenous nations structurally lack.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector, beneficiary,
    institutional, generational, arbitrage, national).

% Administers treaty implementation, land title registries, resource licensing, and litigation defense. Under the stewardship reading it is bound to obtain consent and share governance over territorial resources, but historically enforced a unilateral administrative model instead. It cannot fully exit the treaty relationship (constitutional and international pressure constrain repudiation) but has wide latitude in how loosely or fully it honors stewardship obligations, and bears reputational and fiscal costs if forced into full compliance.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% Inherit whatever state the territory and its ecological base are left in by present extraction decisions. Have no seat in current licensing or governance processes; the stewardship reading's premise — that the land is held in trust for ongoing coexistence rather than consumed — is the standard by which their inherited condition will be judged, but they cannot presently enforce it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_territory, payer,
    powerless, civilizational, trapped, regional).

% Hold private title or municipal jurisdiction within treaty territory under the extinguishment-reading assumption that underlying Indigenous jurisdiction was fully surrendered. Under a stewardship reading their tenure sits atop a jurisdiction that was never extinguished, only shared — a possibility rarely presented to them and largely absent from municipal and provincial planning processes.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_landholders_and_municipalities, excluded,
    moderate, biographical, constrained, local).

% Adjudicate treaty disputes, hear evidence on original treaty intent (oral history, negotiator records, comparative interpretive canons), and can order remedies ranging from compensation to injunctions on resource projects. Their interpretive choices among the three kernel readings materially determine which obligations are enforceable.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, treaty_and_land_rights_tribunals, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the stewardship reading, the treaty coordinates shared occupation and use of a single territory by two distinct polities without either fully displacing the other — resource decisions, land use, and mutual protection obligations are meant to be worked out jointly on an ongoing basis rather than settled once by transfer.
% TRANSFER_FUNCTION: In principle the arrangement moves consultation rights, co-management authority, and a share of resource benefit toward Indigenous treaty nations, and moves a duty of consent-seeking and shared governance onto the settler state. In practice, resource revenue and unilateral administrative control flow toward the settler state and its licensed industries, while Indigenous nations bear the costs of degraded territory, litigation burden, and unimplemented commitments.
% ABSENT_VOICES: Future generations of the treaty territory have no procedural standing in current resource licensing decisions. Many individual Indigenous community members whose oral-history testimony would corroborate the stewardship reading are not systematically brought before tribunals, whose evidentiary standards often privilege settler-state documentary records over oral tradition.
% DISAPPEARANCE_RATIONALE: If the treaty relationship were treated as void rather than persisting under any reading, current resource licensing, land title, and co-management arrangements built partly on treaty recognition would lose their legal basis; litigation, land claims, and existing partial co-management boards would collapse, and the settler state would need an entirely different (likely purely assertion-based) justification for jurisdiction over the territory.
% FOUNDING_PROBLEM: Two polities needed a durable basis for occupying and using the same territory without perpetual warfare — the treaty was negotiated (in the stewardship reading's account) as a relational agreement to share the land and its stewardship, not to transfer it outright.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous treaty nations and many treaty historians and legal scholars (working from negotiator records, wampum belt records, and oral tradition) corroborate the stewardship reading of founding intent. The settler state government and resource-sector beneficiaries largely reject it in practice even where they do not reject it in rhetoric, and treaty tribunals are split — some rulings adopt the stewardship or nation-to-nation reading, others still operate as if extinguishment occurred. No party wholly outside all three interest groups has definitively settled which reading the original parties intended, which is itself the contested kernel this story is one reading of.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that most resource revenue and administrative control from treaty territory currently flow to the settler state and its licensed industries despite the stewardship reading's requirement of joint governance. Suppression (0.72) is high because enforcement of the stewardship reading against a reluctant settler state requires sustained litigation, political mobilization, and international pressure — the reading does not enforce itself. Theater ratio (0.44) captures the substantial gap between consultation processes (frequently performative box-ticking) and genuine co-management authority. Accessibility collapse is moderate (0.4): alternative interpretive frameworks (extinguishment, nation-to-nation) remain live and contested, so alternatives to the current practice have not fully collapsed — this is precisely why three sibling readings persist rather than one settled account. Resistance is high (0.78): Indigenous nations, treaty scholars, and international human rights bodies actively contest the current administrative practice, which is inconsistent with a settled, uncontested arrangement. The one shared time grid shows extraction and suppression both rising over the interval as resource development intensified and the state's administrative apparatus for managing (not fully honoring) treaty claims matured, while theater ratio rose in parallel as consultation processes proliferated without corresponding transfer of governance authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations sit as both nominal beneficiary (they are the rights-holder the stewardship reading protects) and structural payer (they bear the costs of the gap between the reading and practice) — this dual role is why they carry both role and secondary_role. The settler state resource sector is a clean beneficiary with arbitrage-grade exit (capital mobility). The settler state government is agenda-setter with only constrained exit — it cannot repudiate the treaty relationship outright without significant constitutional and international cost, but retains wide discretion in how loosely to interpret its obligations. Future generations are maximally powerless and trapped, bearing costs from decisions in which they have no voice — their directionality sits at the full-target end by structural necessity, not by any exit-option analysis, since they do not yet exist to exercise exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors symmetric to each other: labeling the treaty relationship pure extraction (snare) would erase the genuine coordination function the stewardship reading identifies — two polities do need a durable basis for sharing one territory, and the treaty is not merely a device for settler dispossession even under contested implementation. Conversely, labeling it a pure rope would launder the asymmetric extraction that persists in practice — resource revenue does flow overwhelmingly to the settler state and its licensees, and the coordination story ('we share stewardship') is frequently used as legitimating cover for continued unilateral administration. The tangled_rope reading holds both facts: real coordination function, real asymmetric extraction, requiring active enforcement (litigation, political pressure) to keep the coordination-favoring interpretation alive against administrative and economic pressure toward the extinguishment reading's practical effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_treaty_intent_indeterminacy,
    'Did the original treaty negotiations, as understood by the Indigenous signatory nations at the time, intend land cession, shared stewardship without cession, or an evolving nation-to-nation relationship?',
    'Comparative analysis of negotiator records, oral history testimony preserved through Indigenous knowledge-keepers, wampum belt and other material treaty records, and settler-state documentary records, weighted according to evidentiary canons that do not systematically privilege the documentary tradition of only one party.',
    'If historical evidence strongly corroborates the stewardship reading''s account of original intent, courts and legislatures gain grounds to reclassify current administrative practice as breach rather than discretionary policy, sharply increasing enforceable obligations on the settler state. If evidence is genuinely indeterminate or supports extinguishment, the stewardship reading''s coordination-function claim weakens and the constraint moves toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_treaty_intent_indeterminacy, conceptual, 'Irreducible interpretive uncertainty about which of the three kernel readings reflects the treaty parties'' original mutual understanding.').

omega_variable(
    stewardship_reading_beneficiary_ambiguity,
    'Does the stewardship reading, if fully implemented, genuinely restore net benefit to Indigenous treaty nations, or does its emphasis on ''shared'' stewardship risk legitimating continued settler-state resource access under a softer legitimating narrative than extinguishment provided?',
    'Track implementation outcomes in jurisdictions where courts have adopted a stewardship-adjacent interpretation (co-management boards, revenue-sharing agreements) and measure actual resource-benefit distribution and jurisdictional authority transferred, compared to pre-adoption baselines.',
    'If implementation genuinely shifts resource control and revenue toward Indigenous nations, the stewardship reading functions as intended (rope-leaning tangled rope with narrowing extraction). If implementation mainly adds consultation theater without control transfer, the reading risks becoming a more sophisticated extraction-legitimating device than extinguishment, not a correction of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_reading_beneficiary_ambiguity, empirical, 'Whether declaring beneficiaries under the stewardship reading corresponds to genuine present benefit or is aspirational/contested.').

omega_variable(
    framing_choice_disclosure,
    'Why was the stewardship reading (rather than nation_to_nation or extinguishment) selected as the primary framing for this constraint story, and what would change under an alternative framing?',
    'This is a conceptual (Ω_C) disclosure, not an empirically resolvable question: the stewardship reading was selected because it foregrounds ongoing land-based mutual obligation rather than either a completed-transaction account (extinguishment) or a purely diplomatic sovereign-equality account (nation_to_nation). Under the nation_to_nation framing, the beneficiary/victim structure would shift toward emphasizing formal governmental-to-governmental consent mechanisms and modern treaty-law remedies rather than territorial co-management; under extinguishment, Indigenous nations would drop out of the beneficiary set for jurisdiction entirely and the constraint would likely classify closer to a settled (if historically extractive) mountain or snare rather than a tangled_rope.',
    'Confirms this story''s classification is framing-relative in the sense the kernel-reading structure anticipates: cs_pattern and directionality outputs would differ materially across the three sibling readings, which is precisely why they are authored as three separate constraint stories rather than one story with a measurement parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_choice_disclosure, conceptual, 'Documents which of three coherent kernel framings was chosen and what would change under the alternatives, per the ε-invariance disclosure requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__stewardship_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__stewardship_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__stewardship_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__stewardship_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__stewardship_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__stewardship_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__stewardship_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__stewardship_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__stewardship_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__stewardship_reading, base_extractiveness, 60, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__stewardship_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__stewardship_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__stewardship_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__stewardship_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__stewardship_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the historical treaty' into structurally distinct constraints per the kernel-reading discipline: extinguishment_reading (property-transaction account, sovereignty ceded), nation_to_nation_reading (sovereign-equals diplomatic account, ongoing consent required), and this stewardship_reading (relational land-stewardship account, no cession, mutual coexistence obligations). Each has its own ε, its own beneficiary/victim structure, and its own claimed_type; they are linked via affects_constraints rather than merged because measuring 'the treaty' under different interpretive frames yields different extraction values, victim sets, and enforceability profiles — exactly the case the ε-invariance principle requires decomposing rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
