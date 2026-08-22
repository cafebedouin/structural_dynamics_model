% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Sovereignty as Constitutional Subordination
 *   domain: political_economy/federalism/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the CONSTITUTIONAL SUBORDINATION reading of
 *   the provincial sovereignty boundary. Under this reading, provinces are
 *   created by and subordinate to the federal constitution; they have no
 *   residual or inherent sovereignty; and exit from the federation requires
 *   federal consent, making unilateral separation constitutionally impossible
 *   (ultra vires). This reading vindicates federal authority over climate
 *   policy, equalization transfers, and interprovincial commerce as
 *   legitimate exercises of federal constitutional jurisdiction. The reading
 *   is contested: compact federalism treats provinces as sovereign
 *   contractors with residual authority and exit rights; resource-sovereignty
 *   readings ground provincial autonomy in s.92A resource ownership. The
 *   constraint story describes THE SUBORDINATION READING ONLY (Rule 1), not
 *   the contest itself. The contest enters via omega variables and
 *   cs_structure routing (Rules 2–4). The measurement series shows
 *   extractiveness rising from 0.48 to 0.68 over 30 time units (t=0 to t=30),
 *   then plateauing at 0.68 (t=30 to t=40): this reflects accumulated federal
 *   regulatory scope expansion on climate and equalization, then
 *   stabilization as the reading reached mature institutional entrenchment.
 *   Theater ratio rises from 0.25 to 0.42 over the same interval:
 *   performative constitutional rhetoric ('provinces have no sovereignty')
 *   increasingly dominates enforcement after the initial extraction mechanism
 *   was established, suggesting the reading has become a script for
 *   institutional legitimacy rather than an actively contested principle.
 *
 * KEY AGENTS:
 *   - federal_authority — institutional agenda-setter, controls constitutional interpretation and exit rules (powerful/arbitrage)
 *   - resource_rich_provinces — target, bear equalization extraction and resource-policy subordination (powerful/identity_locked)
 *   - separatist_movements — target, delegitimized by constitutional nullification (moderate/identity_locked)
 *   - constitutional_courts — institutional agenda-setter, enforce the reading via judicial review (institutional/analytical)
 *   - lower_gdp_provinces — beneficiary, receive equalization transfers (organized/constrained)
 *   - civic_federalism_advocates — beneficiary, benefit from federal coordination authority (moderate/mobile)
 *   - indigenous_nations — excluded, trapped between federal/provincial authority (moderate/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.71).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Sovereignty as Constitutional Subordination").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/constitutional_law").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '4bbf54b2-80f4-4120-b7a0-78e3722c0603').
narrative_ontology:cs_kernel_codification('4bbf54b2-80f4-4120-b7a0-78e3722c0603', fixed_text).
narrative_ontology:cs_authority_grounding('4bbf54b2-80f4-4120-b7a0-78e3722c0603', lineage).
narrative_ontology:cs_interpretation_layer_present('4bbf54b2-80f4-4120-b7a0-78e3722c0603').
narrative_ontology:cs_reading_relation('4bbf54b2-80f4-4120-b7a0-78e3722c0603', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('4bbf54b2-80f4-4120-b7a0-78e3722c0603', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('4bbf54b2-80f4-4120-b7a0-78e3722c0603', foundational, provinces_created_creatures_doctrine).
narrative_ontology:cs_axiom_status(provinces_created_creatures_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4bbf54b2-80f4-4120-b7a0-78e3722c0603', provinces_created_creatures_doctrine, deontological).
narrative_ontology:cs_axiom('4bbf54b2-80f4-4120-b7a0-78e3722c0603', foundational, federal_supremacy_enumerated_powers).
narrative_ontology:cs_axiom_status(federal_supremacy_enumerated_powers, holdable).
narrative_ontology:cs_axiom_grounding('4bbf54b2-80f4-4120-b7a0-78e3722c0603', federal_supremacy_enumerated_powers, conventional).
narrative_ontology:cs_axiom('4bbf54b2-80f4-4120-b7a0-78e3722c0603', secondary, exit_requires_constitutional_amendment).
narrative_ontology:cs_axiom_status(exit_requires_constitutional_amendment, holdable).
narrative_ontology:cs_axiom_grounding('4bbf54b2-80f4-4120-b7a0-78e3722c0603', exit_requires_constitutional_amendment, conventional).
narrative_ontology:cs_reference_frame('4bbf54b2-80f4-4120-b7a0-78e3722c0603', hierarchical_federal_constitution).
narrative_ontology:cs_drift_state('4bbf54b2-80f4-4120-b7a0-78e3722c0603', contemporary_resource_sovereignty_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4bbf54b2-80f4-4120-b7a0-78e3722c0603', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_authority).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_coordination_mechanism).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, lower_gdp_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, civic_federalism_advocates).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_spending_power_legitimacy).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_harmonization_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution as granting exclusive jurisdiction over interprovincial commerce, taxation, and national coordination. Sets equalization transfers, climate policy, and exit terms. Claims the authority to deny provincial secession petitions on constitutional grounds, treating separation as ultra vires rather than negotiable. Controls enforcement machinery via courts and legislative legitimacy derived from nationwide democratic mandate.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear disproportionate equalization extraction (revenue redistribution to lower-GDP provinces). Argue they own natural resources under s.92A but lack sovereignty to deploy them without federal veto over carbon policy, interprovincial environmental standards, and climate agreements. Would negotiate exit or sovereignty expansion if exit were an option; exit is denied on constitutional grounds as structurally impossible (not negotiable, not politically feasible).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, excluded).

% Mobilize population around resource grievance or cultural identity to demand exit from the federation. Bear the constraint directly: they are told exit is constitutionally impossible (not illegal, but legally nullified), that democratic referendum on exit is not binding under this reading, and that unilateral declaration of independence is ultra vires. The constraint's enforcement consists of judicial invalidation and denial of recognition rather than overt coercion.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, payer,
    moderate, biographical, identity_locked, regional).

% The subordination reading enables uniform national standards (climate, interprovincial trade, social programs) without individual opt-out. Provinces benefit indirectly from stability, but also lose independent environmental and resource policy space. The constraint makes provinces carriers of federal authority rather than independent sovereigns negotiating with peers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_coordination_mechanism, beneficiary,
    institutional, generational, analytical, national).

% Receive equalization transfers funded by the federal extraction machinery targeting resource-rich provinces. Depend on the subordination reading to maintain interprovincial transfer legitimacy; a compact reading would treat transfers as negotiated across sovereign equals, eroding transfer claims.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, lower_gdp_provinces, beneficiary,
    organized, biographical, constrained, national).

% Are jurisdictionally trapped between federal and provincial authority, with limited sovereignty recognition. The subordination reading keeps both levels empowered to regulate indigenous territory; they would argue for direct sovereignty status independent of both levels, but are excluded from the provincial-federal negotiation entirely.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    moderate, generational, trapped, regional).

% Adjudicate whether exit referenda are binding, whether provincial actions violate federal constitutional authority, and whether equalization is legitimate under the distribution of powers. They enforce the subordination reading via judicial review; they also produce the jurisprudence that legitimates or contests it.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Believe federal coordination on climate, equality, and interprovincial commerce requires subordination of provincial resource autonomy. They benefit from the reading because it protects shared national policy from provincial veto. They do not directly collect from the constraint but advocate its maintenance.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, civic_federalism_advocates, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_authority).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform national standards (climate policy, interprovincial trade rules, equalization transfers) without provincial exit option or per-province opt-out; prevents regulatory arbitrage and ensures national coordination on collective problems (emissions, interprovincial commerce, minimum social standards).
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-rich provinces to lower-GDP provinces via equalization, funded by federal authority's power to set provincial roles. Also transfers policy sovereignty: provinces cannot act unilaterally on resources, environmental standards, or interprovincial trade without federal coordination.
% ABSENT_VOICES: Indigenous nations are structurally excluded (not parties to the provincial-federal negotiation); separatist movements and resource-sovereignty advocates are present but delegitimized (their demands are treated as constitutionally nullified rather than negotiable). Rival federalism readings (compact, resource-sovereignty) are not represented in the institutional decision-making.
% DISAPPEARANCE_RATIONALE: If the subordination reading disappeared and was replaced by compact federalism or resource sovereignty, provinces could negotiate exit terms, retain environmental policy independence, and treat equalization as negotiated transfer rather than constitutional obligation. Federal authority would lose veto power over separation and resource deployment. The fiscal and regulatory architecture would require renegotiation across potentially-sovereign peers rather than hierarchical federal distribution.
% FOUNDING_PROBLEM: Post-Confederation need to coordinate national economy and prevent regulatory balkanization; Dominion government required unitary taxing and spending authority to build transcontinental infrastructure and maintain fiscal coherence across growing number of provinces.
% FOUNDING_PROBLEM_CORROBORATION: Federal authorities and civic federalism advocates attest the founding problem is live: climate coordination, interprovincial trade harmonization, and social program portability require federal authority. Resource-rich provinces and constitutional scholars endorsing compact or resource-sovereignty readings attest the problem has been reframed: modern separation-of-powers doctrine and s.92A resource ownership suggest provinces have moved from subordinate to coordinate status; the 'problem' is now federal overreach. International legal scholars and comparative federalism experts outside both camps note the reading is contested globally (Australia, Germany, and EU models show different resolutions).
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as TANGLED ROPE (not pure snare) because it genuinely coordinates national standards (climate, trade, social portability) AND extracts from resource provinces through equalization and policy subordination. Extraction is measured at 0.68 at the interval end, reflecting the disproportionate fiscal burden on resource-rich provinces and their loss of independent environmental policy sovereignty. Suppression is 0.71, slightly higher than extraction, because the constraint's persistence depends on actively denying exit legitimacy (constitutional courts invalidate separation referenda, federal veto over provincial resource policy, and judicial prevention of unilateral declaration of independence). Theater is 0.42 — moderate-low — because the coordination function is genuine (climate coordination, interprovincial trade rules have real welfare effects) but an increasing share of enforcement energy defends the subordination principle itself rather than the substantive coordination function. The measurement trajectory shows extraction accelerating through t=30 as federal environmental regulations and equalization formulas expanded, then stabilizing as the reading matured and reached its equilibrium institutional form. The theater ratio rises concurrently, suggesting that as the technical coordination was established, rhetorical maintenance of the 'provinces are subordinate creatures' narrative became proportionally more costly (more of enforcement went to legitimacy-work than to substantive policy).
 *
 * PERSPECTIVAL GAP:
 *   The federal seat and the resource-province seat compute different types from this same structural data. From the federal seat (and civic federalism advocates), the constraint is a genuine coordination mechanism enabling national climate and fiscal policy — the extraction is the price of coordination, not extractive overhead. Effective extraction from the federal seat's perspective approaches 0 (beneficiary directionality). From the resource-province seat, the same constraint is experienced as enforced subordination: they pay equalization without negotiating its terms, they cannot deploy resources independently, and they are denied exit (identity-locked, no arbitrage option). Effective extraction from this seat's perspective approaches 0.95 (target directionality, trapped/identity_locked, high power but constrained by constitutional rule). The engine computes this divergence from the beneficiary/victim declarations and directionality atoms; the authored claim (tangled rope) reflects the structural reality that BOTH functions are present (genuine coordination AND asymmetric extraction), which is exactly what makes it tangled rather than pure rope or pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority is the beneficiary (collects policy authority, controls exit terms, sets equalization rates — directionality ≈ 0.1, low/beneficiary end). Resource-rich provinces are the target (pay equalization, lose resource sovereignty, denied exit — directionality ≈ 0.92, high/target end, identity_locked amplifying the target position). Lower-GDP provinces sit near symmetric (0.5) — they receive equalization transfers but lose independent policy autonomy and depend on federal coordination. Separatist movements are targets (directionality ≈ 0.88, identity_locked, denied exit, and their core claim is delegitimized). Indigenous nations are trapped but not clearly extracted from or beneficiaries of THIS constraint (it orthogonally traps them between the two levels); their directionality would be computed separately if a story specifically decomposed indigenous-federal/provincial relations. No directionality overrides are needed; the structural data (beneficiaries: federal authority + interprovincial mechanism; victims: resource provinces + separatists) derives the right d values via the canonical derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Confederation need for national economic coordination and infrastructure) was live through the mid-20th century. By 2000–2010, the problem statement had shifted: the question became whether climate coordination and interprovincial trade harmonization required provincial subordination or could be achieved through coordinate sovereign negotiation. This reading answers the question by asserting that subordination IS the only framework that makes national standards binding. Resource provinces increasingly contest this framing, arguing that s.92A resource ownership grounds sufficient provincial sovereignty to negotiate equalization and climate terms as equals rather than subordinates. The constraint now persists as institutional inertia (courts apply subordination doctrine because it is established precedent) and as federal capture (federal institutions benefit from expanded authority under the reading). The mandatrophy signal is moderate: the coordination function is genuine enough to sustain the rope half, but the extraction has accumulated and the theater ratio has risen, suggesting that performance of the subordination principle is increasingly propping up what might otherwise renegotiate toward compact federalism. A future shift toward interprovincial negotiation (rather than hierarchical federal distribution) would resolve the mandatrophy by clarifying the founding problem as obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hierarchical_vs_coordinate_constitution,
    'Does the written Constitution establish a hierarchical (subordination) or coordinate (compact) structure of provincial-federal relations?',
    'Constitutional interpretation scholarship, comparative federalism analysis, and counterfactual: if s.92A was intended to establish provincial sovereignty, why did courts consistently subordinate resource policy to federal environmental and trade authority post-1982?',
    'If the Constitution is better read as coordinate, the constraint type shifts from tangled_rope (federation coordinator extracting from resource provinces) toward rope or compact (mutual coordination among sovereigns). If hierarchical, the constraint holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hierarchical_vs_coordinate_constitution, conceptual, 'Whether the Constitution establishes hierarchy or coordination between federal and provincial authority.').

omega_variable(
    exit_legitimacy_boundary,
    'Is the denial of exit (constitutional impossibility of unilateral separation) a structural feature of any federation, or a contingent choice this particular reading enforces?',
    'Comparative federalism: Australia, Switzerland, Germany, and EU models each permit different exit mechanisms (constitutional amendment, state law, negotiated withdrawal). Empirical test: does a constitution that permits negotiated exit (rather than forbidding it) produce materially different federal stability or policy outcomes?',
    'If exit denial is merely this reading''s choice (not a structural necessity), then alternative readings permitting negotiated exit are not logically foreclosed; the constraint becomes more clearly extractive (blocking exit is the mechanism, not a side effect of coordination). If exit denial is structurally necessary for federation, the constraint''s suppression is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_legitimacy_boundary, empirical, 'Whether denying exit is necessary for federalism or a contingent institutional choice.').

omega_variable(
    equalization_vs_extraction_legitimacy,
    'Does equalization represent genuine redistribution to equalize opportunity across the federation, or does it represent extraction from resource provinces to benefit federal authority and lower-GDP provinces?',
    'Fiscal analysis comparing equalization transfers to cost of national coordination services (climate, trade administration); regional GDP growth trajectories post-equalization; provincial policy autonomy remaining after equalization constraints.',
    'If equalization exceeds the cost of coordination, the constraint has more extractive overhead than justified by coordination; if it tracks cost, the extraction is coordination cost rather than rent. This affects classification between tangled_rope (extraction justified by coordination) and snare (extraction exceeds coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equalization_vs_extraction_legitimacy, empirical, 'Whether equalization represents fair coordination cost or excess extraction.').

omega_variable(
    identity_lock_vs_rational_choice,
    'Do resource-rich provinces and separatist movements remain within the federation because the subordination reading is structurally binding (identity-locked: they cannot conceive of exit or the exit is truly impossible), or because they are rationally choosing to remain given high exit costs?',
    'Post-exit trajectory analysis from actual separations (Quebec referenda aftermath, Catalonia post-independence bid, Scotland post-Brexit): if exit barriers dissolve, do provinces/regions immediately pursue it (identity-lock broken), or do they renegotiate terms and often remain (rational cost-benefit adjustment)?',
    'If identity-locked, suppression is internalized and persistent even if formal barriers were removed; if rational-choice, suppression is structural (costly exit) rather than internalized. This affects the mechanism omega and informs post-exit trajectories (separatist movements would need identity reframing, not just cost reduction, to exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_choice, empirical, 'Whether provincial attachment to the federation is structural/identity or rational/cost-based.').

omega_variable(
    subordination_reading_foreclosure_status,
    'Does the subordination reading logically foreclose the compact federalism reading, or do both remain coherent within distinct institutional frameworks?',
    'If a single party (e.g., a provincial government) adopted both readings simultaneously (treating provinces as subordinate for climate policy but as sovereign contractors for resource negotiation), would this be internally contradictory or merely expedient?',
    'If foreclosure, the readings are in genuine logical contradiction; if coexistence, they are held by different institutional actors and represent a live contest rather than resolved question. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subordination_reading_foreclosure_status, conceptual, 'Logical relationship between subordination and compact federalism readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_const_sub_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(prov_const_sub_tr_t0, observed).
narrative_ontology:measurement(prov_const_sub_tr_t5, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(prov_const_sub_tr_t5, observed).
narrative_ontology:measurement(prov_const_sub_tr_t10, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(prov_const_sub_tr_t10, observed).
narrative_ontology:measurement(prov_const_sub_tr_t15, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(prov_const_sub_tr_t15, observed).
narrative_ontology:measurement(prov_const_sub_tr_t20, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(prov_const_sub_tr_t20, observed).
narrative_ontology:measurement(prov_const_sub_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(prov_const_sub_tr_t25, observed).
narrative_ontology:measurement(prov_const_sub_tr_t30, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(prov_const_sub_tr_t30, observed).
narrative_ontology:measurement(prov_const_sub_tr_t40, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(prov_const_sub_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prov_const_sub_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prov_const_sub_be_t0, observed).
narrative_ontology:measurement(prov_const_sub_be_t5, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(prov_const_sub_be_t5, observed).
narrative_ontology:measurement(prov_const_sub_be_t10, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(prov_const_sub_be_t10, observed).
narrative_ontology:measurement(prov_const_sub_be_t15, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(prov_const_sub_be_t15, observed).
narrative_ontology:measurement(prov_const_sub_be_t20, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(prov_const_sub_be_t20, observed).
narrative_ontology:measurement(prov_const_sub_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(prov_const_sub_be_t25, observed).
narrative_ontology:measurement(prov_const_sub_be_t30, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(prov_const_sub_be_t30, observed).
narrative_ontology:measurement(prov_const_sub_be_t40, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(prov_const_sub_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_const_sub_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prov_const_sub_su_t0, observed).
narrative_ontology:measurement(prov_const_sub_su_t5, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(prov_const_sub_su_t5, observed).
narrative_ontology:measurement(prov_const_sub_su_t10, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(prov_const_sub_su_t10, observed).
narrative_ontology:measurement(prov_const_sub_su_t15, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(prov_const_sub_su_t15, observed).
narrative_ontology:measurement(prov_const_sub_su_t20, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(prov_const_sub_su_t20, observed).
narrative_ontology:measurement(prov_const_sub_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(prov_const_sub_su_t25, observed).
narrative_ontology:measurement(prov_const_sub_su_t30, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(prov_const_sub_su_t30, observed).
narrative_ontology:measurement(prov_const_sub_su_t40, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(prov_const_sub_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_transfer_legitimacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_climate_authority).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_trade_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the provincial_sovereignty_boundary kernel. All three readings share the same referent (the de jure and de facto authority structure of federal-provincial relations) but instantiate different constraints depending on which reading interprets the Constitution. The subordination reading (this story) treats federal authority as supreme and provincial subordination as structurally necessary; compact federalism treats both as coordinate sovereigns with negotiable terms; resource-sovereignty treats provincial resource ownership as establishing absolute provincial sovereignty. Each reading has different ε, beneficiaries/victims, and type. The three stories are linked via network.affects_constraints as a constraint family. Sibling stories: constraint_provincial_sovereignty_boundary__compact_federalism and constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy. The subordination reading forecloses compact within any single unified institutional framework but coexists with it across institutional actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__constitutional_subordination, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
