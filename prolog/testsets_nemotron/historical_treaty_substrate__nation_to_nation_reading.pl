% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Treaties as Nation-to-Nation International Agreements
 *   domain: legal/anthropological/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the nation-to-nation reading of the
 *   historical_treaty_substrate kernel. It treats historical treaties (e.g.,
 *   Numbered Treaties 1-11 in Canada, Treaty of Waitangi in NZ, US-Indian
 *   treaties) as binding international agreements between sovereign equals
 *   that require ongoing free, prior, and informed consent for territorial
 *   changes and resource decisions. The reading asserts that Indigenous
 *   nations did not cede sovereignty but agreed to share the land under
 *   specific terms. The settler state is bound by international treaty law
 *   (pacta sunt servanda) and domestic constitutional law (s.35 Constitution
 *   Act 1982, Treaty of Waitangi Act 1975). Unilateral resource extraction by
 *   the state or private actors violates the treaty. The constraint operates
 *   as a tangled rope: it coordinates peaceful coexistence (genuine
 *   coordination function) while extracting compliance costs from the settler
 *   state and extractive interests (asymmetric extraction), requiring active
 *   enforcement through courts, consultation processes, and international
 *   monitoring.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.15).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.25).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Treaties as Nation-to-Nation International Agreements").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/anthropological/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '31421bf3-e677-4530-aaef-1645f645b2b3').
narrative_ontology:cs_kernel_codification('31421bf3-e677-4530-aaef-1645f645b2b3', formalized).
narrative_ontology:cs_authority_grounding('31421bf3-e677-4530-aaef-1645f645b2b3', lineage).
narrative_ontology:cs_interpretation_layer_present('31421bf3-e677-4530-aaef-1645f645b2b3').
narrative_ontology:cs_reading_relation('31421bf3-e677-4530-aaef-1645f645b2b3', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('31421bf3-e677-4530-aaef-1645f645b2b3', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('31421bf3-e677-4530-aaef-1645f645b2b3', foundational, treaties_as_international_agreements).
narrative_ontology:cs_axiom_status(treaties_as_international_agreements, holdable).
narrative_ontology:cs_axiom_grounding('31421bf3-e677-4530-aaef-1645f645b2b3', treaties_as_international_agreements, conventional).
narrative_ontology:cs_axiom('31421bf3-e677-4530-aaef-1645f645b2b3', foundational, indigenous_sovereignty_unextinguished).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_unextinguished, holdable).
narrative_ontology:cs_axiom_grounding('31421bf3-e677-4530-aaef-1645f645b2b3', indigenous_sovereignty_unextinguished, deontological).
narrative_ontology:cs_axiom('31421bf3-e677-4530-aaef-1645f645b2b3', foundational, ongoing_consent_required).
narrative_ontology:cs_axiom_status(ongoing_consent_required, holdable).
narrative_ontology:cs_axiom_grounding('31421bf3-e677-4530-aaef-1645f645b2b3', ongoing_consent_required, conventional).
narrative_ontology:cs_reference_frame('31421bf3-e677-4530-aaef-1645f645b2b3', treaty_making_as_sovereign_act).
narrative_ontology:cs_drift_state('31421bf3-e677-4530-aaef-1645f645b2b3', contemporary_undrip_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('31421bf3-e677-4530-aaef-1645f645b2b3', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_as_constrained_party).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_legal_order).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_extractive_interests).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, unilateral_resource_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_as_constrained_party).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, international_treaty_law_principles).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, indigenous_sovereignty_continuity).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, free_prior_informed_consent).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, pacta_sunt_servanda).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter treaties as co-equal sovereign parties with ongoing consent rights over territorial changes and resource decisions. Their sovereignty is not ceded but recognized through the treaty relationship. Exit from the treaty framework would mean loss of internationally recognized legal protections, but their identity as sovereign nations is fused with the treaty relationship itself — the treaties constitute their international legal personality. They bear the burden of continually asserting treaty rights against state encroachment.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, continental).

% The settler state (e.g., Canada, US, NZ, Australia) administers the treaty relationship through domestic law, courts, and bureaucracy. It is constrained by treaty obligations requiring consent for resource extraction and territorial changes. It bears the fiscal and political costs of implementing treaty rights (consultation processes, revenue sharing, land returns). It cannot exit the treaty relationship without collapsing its own constitutional legitimacy, but its institutional incentives often pull toward minimizing treaty obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_as_constrained_party, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_as_constrained_party, payer).

% Resource extraction companies, forestry, mining, energy sectors that seek unilateral access to treaty territories. They bear the costs of treaty compliance (consultation, accommodation, revenue sharing, project delays/cancellation). They can exit by moving capital to non-treaty jurisdictions or pressuring the state to extinguish treaty rights. They are not parties to the treaties but are the primary economic actors whose interests the treaty constrains.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_extractive_interests, payer,
    powerful, biographical, mobile, continental).

% Smaller-scale developers, agricultural interests, infrastructure projects that treat treaty territories as open for unilateral development. They experience treaty requirements as regulatory barriers. Unlike major extractive interests, they lack the political capital to lobby for systemic treaty revision; their exit is individual project abandonment or regulatory arbitrage.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, unilateral_resource_developers, payer,
    moderate, immediate, mobile, local).

% The system of international law (UNDRIP, ICCPR, ICESCR, treaty law, customary law) that treats these treaties as binding international agreements. It benefits from the treaties as precedents for indigenous-state relations globally. It has no material exit — it is the normative framework itself. Its 'situation' is the coherence and credibility of the international legal system.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, international_legal_order).

% The pre-existing and continuing legal orders of Indigenous nations that govern their internal relations and their external relations including treaty-making. These orders are vindicated when treaties are recognized as nation-to-nation agreements. They have no exit — they are the constitutive law of the nations themselves.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_legal_orders, beneficiary,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__nation_to_nation_reading, indigenous_legal_orders).

% National courts (Supreme Court of Canada, US Supreme Court, NZ Courts, High Court of Australia) that adjudicate treaty rights. They set the domestic legal agenda for treaty interpretation. They are constrained by constitutional structure, precedent, and political legitimacy. They cannot easily exit their role without constitutional crisis.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, agenda_setter,
    institutional, generational, constrained, national).

% Scholars in legal anthropology, Indigenous law, comparative constitutional theory, international law who analyze the treaty relationship. They bear no material costs or benefits from the constraint's operation. Their exit is intellectual — they can choose other research topics.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, academic_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates peaceful coexistence and resource sharing between Indigenous nations and settler states across shared territories, replacing conquest with negotiated consent. Solves the problem of how two sovereign political orders share one land base without one extinguishing the other.
% TRANSFER_FUNCTION: Moves decision-making authority over territorial changes and resource extraction from unilateral state control to a consent-based process requiring Indigenous nation agreement. Moves a share of resource revenues and jurisdictional authority from the state (and private extractive interests) to Indigenous nations.
% ABSENT_VOICES: Future generations of both Indigenous nations and settler populations who will inherit the treaty relationship but cannot participate in its current interpretation. Non-human entities (waters, lands, animal nations) recognized as parties in many Indigenous legal orders but excluded from state legal frameworks. Diasporic Indigenous peoples displaced from treaty territories.
% DISAPPEARANCE_RATIONALE: If the nation-to-nation reading vanished overnight, the legal framework constraining unilateral state action would collapse. Resource extraction would proceed without consent requirements. Indigenous nations would lose their primary international-law foothold for territorial authority. The settler state's constitutional legitimacy (which rests partly on treaty-making) would face existential crisis. The international legal order would lose a key precedent for indigenous-state relations.
% FOUNDING_PROBLEM: How to establish peaceful, lawful relations between Indigenous sovereign nations and arriving European powers without conquest or cession of Indigenous sovereignty — replacing the doctrine of discovery and terra nullius with negotiated consent.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: the Royal Proclamation of 1763 (British Crown), the Treaty of Waitangi (Maori and Crown), the Numbered Treaties' oral histories (Indigenous elders and knowledge-keepers), the Marshall Trilogy (US Supreme Court), the Calder decision (Supreme Court of Canada), UNDRIP (UN General Assembly), and the ongoing treaty negotiation processes in Canada (modern treaties) and New Zealand (Waitangi Tribunal). The founding problem is attested from OUTSIDE the benefiting parties by the settler states' own founding legal instruments and by the international legal order.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).
:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) at the interval end because this reading's own lights assess the standing arrangement as low-extraction — the treaty constraint primarily coordinates, and the extraction experienced by the settler state and extractive interests is the cost of coordination, not rent-seeking. However, the temporal series shows high extraction (0.65-0.7) during the peak suppression era (1876-1950: Indian Act, residential schools, pass system, extinguishment policies) when the state actively violated the treaty coordination function. The current low extractiveness reflects the reading's assessment that the constraint, properly implemented, is not extractive toward Indigenous nations. Suppression (0.25) reflects ongoing state resistance to full implementation (duty to consult often reduced to procedural box-checking; consent rarely treated as veto). Theater ratio (0.3) reflects the gap between rhetorical recognition of nation-to-nation relationships and substantive implementation. Accessibility collapse (0.4) is moderate — alternatives (extinguishment, stewardship readings) remain live and contested. Resistance (0.65) is high — Indigenous nations have maintained continuous resistance to treaty violation for 300+ years.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat classifications: from Indigenous nations' seat, the constraint appears as rope (genuine coordination, low extraction). From extractive interests' seat, it appears as snare (coercive suppression of their access). From the settler state's seat, it appears as tangled rope (both coordination benefit and extraction cost). This divergence IS the measurement — the constraint's classification depends on which sovereign's perspective is centered.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are structural beneficiaries (d near 0.0): the treaty constraint subsidizes their sovereignty and territorial authority. The settler state as constrained party sits near symmetric (d ~ 0.5): it bears implementation costs but gains constitutional legitimacy and peaceful relations. Settler state extractive interests and unilateral developers are targets (d near 1.0): they bear the consent requirement as cost. International legal order and Indigenous legal orders are non-agent beneficiaries — they collect no rents but the constraint vindicates their normative frameworks. Domestic courts are agenda-setters with constrained exit — they interpret but cannot unilaterally rewrite the treaty relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The nation-to-nation reading prevents mislabeling the treaty relationship as pure extraction (extinguishment reading) or as purely relational without legal force (stewardship reading). It names the coordination function (peaceful coexistence through consent) and the asymmetric extraction (state/industry bears consent costs) without collapsing either. The mandatrophy risk is that the coordination function atrophies into performance (theater) while the extraction function (state resource revenue) persists — the current theater_ratio of 0.3 signals this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Does the historical treaty substrate instantiate a single kernel with multiple readings, or are these structurally distinct constraints that share only a label?',
    'Test ε-invariance: if the extinguishment reading and nation-to-nation reading produce fundamentally different ε values for the same historical treaties under their own lights, they are distinct constraints per DP-001. The nation-to-nation reading assesses ε=0.15 (low, coordination-dominant); the extinguishment reading would assess ε=0.7+ (high, extraction-dominant) for the same treaties. This ε-divergence confirms they are distinct constraints linked by network.affects_constraints, not measurement variants of one constraint.',
    'If they are distinct constraints, each gets its own classification, stakeholders, and temporal dynamics. The kernel concept becomes an analytical grouping device, not a single constraint with variable measurement. The network edges map the structural influence: extinguishment_reading historically suppressed nation_to_nation_reading; nation_to_nation_reading now structurally challenges extinguishment_reading''s legal force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the treaty substrate is one kernel with contested readings or multiple ε-invariant constraints.').

omega_variable(
    consent_as_veto_ambiguity,
    'Does ''ongoing consent'' in this reading entail a veto right for Indigenous nations over resource projects, or a procedural duty to consult without substantive veto?',
    'Supreme Court of Canada jurisprudence evolution (Haida 2004 → Tsilhqot''in 2014 → Reference re Greenhouse Gas 2021), UNDRIP Art. 32.2 implementation legislation (Canada''s UNDRIP Act 2021), Waitangi Tribunal findings. The structural test: if consent can be withheld without triggering state override, it functions as veto; if state can proceed despite refusal, it is procedural.',
    'If consent = veto, the constraint''s coordination function is stronger (genuine shared decision-making) and extraction from extractive interests is higher (projects can be blocked). If consent = procedure, the constraint leans toward theater (coordination performance without substantive power shift). This directly affects theater_ratio and extractiveness metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_veto_ambiguity, empirical, 'Whether the consent right in this reading is substantive veto or procedural consultation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.25 at interval end) primarily structural (state non-compliance, legislative override) or internalized (Indigenous nations'' capacity to enforce treaty rights eroded by dependency on state funding, legal frameworks, and recognition processes)?',
    'Post-assertion suppression trajectory: if Indigenous nations successfully assert treaty rights (e.g., Tsilhqot''in title declaration) but suppression persists in implementation (delayed revenue sharing, narrow consultation), the suppression is structural. If suppression diminishes when Indigenous legal orders are resourced independently of state funding, internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests — the target (Indigenous nations) carries the suppression internally even after legal victories. This would increase the effective extraction on the Indigenous nation seat despite the reading''s low base ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the treaty relationship.').

omega_variable(
    settler_state_dual_role_coherence,
    'Can the settler state coherently occupy both agenda_setter (administers treaty implementation) and payer (bears implementation costs) roles, or does this dual role mask a structural contradiction where the state''s institutional incentives pull toward minimizing the very obligations it administers?',
    'Institutional analysis of Canada''s Crown-Indigenous Relations department, US Bureau of Indian Affairs, NZ Crown Law Office — do their mandate, budget, and career incentives align with treaty implementation or with state fiscal/territorial interest? Compare outcomes when treaty implementation is administered by an independent body vs. state executive.',
    'If the dual role is incoherent, the settler state should be split into two stakeholders: settler_state_administrator (agenda_setter, low extraction) and settler_state_fiscus (payer, high extraction). This would change the directionality derivation and seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_state_dual_role_coherence, conceptual, 'Coherence of the settler state''s dual structural position in the treaty constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1700, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1700, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hist_tr_t1763, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1763, 0.15).
narrative_ontology:measurement(hist_tr_t1800, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(hist_tr_t1876, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1876, 0.6).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1950, 0.75).
narrative_ontology:measurement(hist_tr_t1973, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1973, 0.55).
narrative_ontology:measurement(hist_tr_t1982, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1982, 0.45).
narrative_ontology:measurement(hist_tr_t2007, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(hist_be_t1700, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(hist_be_t1763, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1763, 0.1).
narrative_ontology:measurement(hist_be_t1800, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(hist_be_t1876, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1876, 0.55).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(hist_be_t1973, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(hist_be_t1982, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(hist_be_t2007, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2007, 0.25).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1700, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hist_su_t1763, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1763, 0.15).
narrative_ontology:measurement(hist_su_t1800, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(hist_su_t1876, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1876, 0.7).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(hist_su_t1973, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(hist_su_t1982, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(hist_su_t2007, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2007, 0.3).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, modern_treaty_negotiation_process).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, undrip_implementation).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, resource_revenue_sharing_regimes).

% DUAL FORMULATION NOTE:
% This constraint (nation_to_nation_reading) and its siblings (extinguishment_reading, stewardship_reading) form a constraint family decomposing the historical_treaty_substrate kernel. The ε-invariance principle requires separate stories because each reading assesses fundamentally different ε values for the same historical treaties: nation-to-nation reads ε≈0.15 (coordination-dominant), extinguishment reads ε≈0.7+ (extraction-dominant), stewardship reads ε≈0.25 (relational coordination). They share the same historical referent (the treaties themselves) but instantiate different constraints with different beneficiary/victim structures, different claimed types, and different temporal dynamics. The network.affects_constraints edges map the structural influence: extinguishment_reading historically enforced suppression of nation_to_nation_reading (1876-1973); nation_to_nation_reading now structurally challenges extinguishment_reading's legal force through court victories and UNDRIP.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, institutional, 0.45).
constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, powerful, 0.9).
constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, moderate, 0.85).
constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
