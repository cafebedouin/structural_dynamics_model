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
 *   human_readable: Constitutional Subordination: Provinces as Federal Creatures
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_subordination reading of
 *   the contested kernel provincial_sovereignty_boundary. Under this reading,
 *   provinces possess no inherent sovereignty but are instead creations of
 *   the federal constitution—their powers are delegated, not reserved. Exit
 *   from the federation requires federal constitutional consent, making
 *   unilateral secession constitutionally void. The constraint operates
 *   through multiple enforcement mechanisms: the courts interpret the
 *   constitutional text to affirm federal supremacy; the amendment process
 *   (requiring federal approval) locks provinces into the framework; and
 *   federal taxation and regulatory authority over interprovincial commerce
 *   allow the federal government to extract resource wealth and impose
 *   pan-Canadian policy (climate, equalization) on provincial governments.
 *   The constraint is CLAIMED as tangled_rope (genuine coordination with
 *   asymmetric extraction) while metrics show substantial suppression and
 *   rising theater—a divergence the engine will measure.
 *
 * KEY AGENTS:
 *   - Federal government: sets constitutional framework, enforces federal supremacy, interprets sovereignty boundary via courts, collects equalization transfers
 *   - Resource-rich provinces (Alberta, Saskatchewan, British Columbia, Newfoundland/Labrador): bear extraction costs through revenue loss, regulatory constraint on resource extraction and environmental policy, constitutional inability to exit
 *   - Equalization-recipient provinces: benefit from fiscal transfers funded by federal taxation of resource wealth; structurally dependent on resource-rich provinces remaining in the federation
 *   - Federal courts: interpret constitutional text, have repeatedly affirmed federal supremacy, treat unilateral secession as unconstitutional
 *   - Separatist movements: excluded from legitimate constitutional discourse, referenda lack constitutional standing, federal consent requirement makes their claims void
 *   - Indigenous nations: predate both sovereignties, are structurally excluded from the federal/provincial sovereignty discussion despite territorial claims
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
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination: Provinces as Federal Creatures").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/constitutional").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '397bd09b-3091-4df2-9dc8-80895be9c01b').
narrative_ontology:cs_kernel_codification('397bd09b-3091-4df2-9dc8-80895be9c01b', fixed_text).
narrative_ontology:cs_authority_grounding('397bd09b-3091-4df2-9dc8-80895be9c01b', extraction).
narrative_ontology:cs_interpretation_layer_present('397bd09b-3091-4df2-9dc8-80895be9c01b').
narrative_ontology:cs_reading_relation('397bd09b-3091-4df2-9dc8-80895be9c01b', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_reading_relation('397bd09b-3091-4df2-9dc8-80895be9c01b', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('397bd09b-3091-4df2-9dc8-80895be9c01b', foundational, federal_supremacy_doctrine).
narrative_ontology:cs_axiom_status(federal_supremacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('397bd09b-3091-4df2-9dc8-80895be9c01b', federal_supremacy_doctrine, conventional).
narrative_ontology:cs_axiom('397bd09b-3091-4df2-9dc8-80895be9c01b', foundational, constitutional_hierarchy_principle).
narrative_ontology:cs_axiom_status(constitutional_hierarchy_principle, holdable).
narrative_ontology:cs_axiom_grounding('397bd09b-3091-4df2-9dc8-80895be9c01b', constitutional_hierarchy_principle, conventional).
narrative_ontology:cs_reference_frame('397bd09b-3091-4df2-9dc8-80895be9c01b', federal_constitutional_hierarchy).
narrative_ontology:cs_drift_state('397bd09b-3091-4df2-9dc8-80895be9c01b', contemporary_climate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('397bd09b-3091-4df2-9dc8-80895be9c01b', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, pan_canadian_policy_beneficiaries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_legislatures).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, constitutional_hierarchy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional framework that defines provincial powers, enforces federal supremacy in enumerated areas (trade, defense, currency), can override provincial environmental and resource policy through federal jurisdiction over interprovincial commerce and international treaties. Controls the amendment mechanism and holds the legal authority to interpret the boundary between federal and provincial competence through courts it appoints. Directly benefits from resource wealth through equalization payments and interprovincial redistribution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the costs of subordination: resource revenues subject to federal taxation and equalization transfers; climate and environmental policy constrained by federal authority over interprovincial trade (carbon pricing mandates, pipeline regulation); exit from the federation requires federal consent, treating territorial claims as constitutionally void. Yet they also benefit from access to the pan-Canadian market, federal infrastructure investment, and national security guarantees. Identity fusion with provincial governance structures makes exit psychologically and institutionally costly even when economically tempting.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, beneficiary).

% Receive fiscal transfers funded by federal taxation of resource-rich provinces; the constitutional subordination framework legitimates this redistribution as a federal power. Their welfare depends on resource provinces remaining within the federation and remaining obligated to contribute. They have limited ability to exit the federation unilaterally and are structured as beneficiaries of the constraint rather than targets.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_recipient_provinces, beneficiary,
    moderate, biographical, constrained, national).

% Interpret and adjudicate the boundary between federal and provincial authority; have repeatedly affirmed federal supremacy in cases of conflict (Secession Reference 1998, pipeline cases). Their authority derives from the same constitutional text but they exercise it to construe the constraint itself. Formally neutral but structurally positioned to defend the constitutional hierarchy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_courts, observer,
    institutional, generational, analytical, national).

% Possess enumerated powers (s.92 Constitution Act 1867) but operate within a framework that treats them as delegated, not sovereign. Can legislate on property and civil rights, education, health—but cannot override federal authority in enumerated areas or claim exemption from federal laws of general application. Face recurring constitutional conflict over resource management, environmental regulation, and interprovincial trade barriers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_legislatures, payer,
    organized, generational, constrained, regional).

% Advocate for provincial/regional independence and claim indigenous or historical sovereignty. Are structurally excluded from legitimate constitutional discourse—referenda can be held but lack constitutional standing, courts have ruled unilateral secession unconstitutional (Secession Reference), and federal consent is legally required. Their exclusion from the amendment process is the enforcement mechanism itself.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded,
    moderate, biographical, trapped, regional).

% Predate both federal and provincial sovereignty claims; are recognized in the Constitution (s.35, s.91(24)) but as subjects of federal and provincial jurisdiction, not as sovereigns. Provincial subordination to federal authority occurs on territories that indigenous nations claim as their own; their voice in the sovereignty debate is structurally excluded from the federal/provincial constitutional conversation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    powerless, civilizational, trapped, local).

% Interact with Canada as a unitary sovereign state through federal government channels; recognize only the federal government as the legitimate negotiating partner. Provincial claims to international treaty-making or trade authority are not recognized; the constitutional subordination framework ensures provinces cannot conduct independent foreign policy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, international_governments, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and market framework across provincial boundaries: one currency, unified commercial law, interprovincial commerce without barriers, national defense and foreign policy coordination, redistributive transfers from resource-rich to poorer regions.
% TRANSFER_FUNCTION: Moves authority over resource extraction, taxation, and environmental policy from provinces to the federal level; moves wealth from resource-rich provinces to equalization-recipient provinces and federal coffers; moves regulatory authority over interprovincial trade and climate policy to federal agencies.
% ABSENT_VOICES: Indigenous nations whose territorial sovereignty claims predate both federal and provincial assertions; separatist movements legally excluded from the amendment process; international actors (other states, global institutions) who recognize only the federal government as sovereign and cannot negotiate with provinces directly.
% DISAPPEARANCE_RATIONALE: If the constitutional subordination constraint vanished, provinces would immediately claim sovereignty over natural resources, environmental policy would fragment into provincial systems, equalization transfers would cease, interprovincial trade barriers would emerge, and the federation would either dissolve or reorganize as a genuine compact with exit clauses. The entire fiscal and regulatory architecture depends on this constraint.
% FOUNDING_PROBLEM: Post-1867 (Confederation): the need for a unified North American state capable of defending against U.S. expansion, developing transcontinental infrastructure, and managing interprovincial resource competition without fragmentation into separate colonial entities.
% FOUNDING_PROBLEM_CORROBORATION: Federal government and pan-Canadian policy advocates attest the unified framework is still necessary for climate coordination, trade integration, and fiscal stability. Resource-rich provinces and separatist movements attest the founding problem (external threat, infrastructure coordination) is resolved and the constraint now persists as federal rent extraction. Academic historians (outside the benefiting parties) document the founding problem as real in 1867 but note its salience has declined with 20th-century security integrations (NATO, continental defense) and that persistent federal control of resources is now justified by different rationales (pan-Canadian climate goals, equalization) rather than the original ones.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.48 to 0.68 over the interval, reflecting increasing federal use of regulatory authority (carbon pricing, pipeline regulation, interprovincial trade rules) to direct provincial resource policy toward federal objectives—the coordination function (unified market, national defense) is real, but extraction has accumulated above coordination cost. Theater rises from 0.25 to 0.42: constitutional rhetoric emphasizes equalization and pan-Canadian goals, but enforcement machinery increasingly serves federal rent capture and power consolidation. Suppression rises from 0.55 to 0.71 because separatist political movements have grown and the federal government has intensified interpretive enforcement through courts and regulatory expansion to maintain the boundary. Accessibility collapse is high (0.79) because the constitutional text, courts, and amendment mechanism create near-complete suppression of exit alternatives—provinces are trapped within the framework. Resistance is moderate (0.58) because separatist movements and resource-province governments mount real political resistance, yet lack formal constitutional standing to change the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agenda-setter seat, the constraint is genuine coordination with asymmetric cost-sharing: provinces benefit from national infrastructure, security, market integration, and receive equalization transfers; federal extraction is the price of coordination. From resource-rich province seats, it is coercive subordination: they fund equalization while losing control of resource policy and cannot exit constitutionally. The engine computes this divergence from stakeholder power levels, exit options (federal: arbitrage, provinces: identity_locked), and beneficiary/victim structure. Identity lock is key: provincial elites are constitutionally tied to the provincial state and cannot defect to federal structures; federal elites have no equivalent loyalty constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government: d ≈ 0.1 (full beneficiary)—sets rules, collects transfers, has arbitrage options, no identity lock. Resource-rich provinces: d ≈ 0.85 (near-full target)—pay extraction, constrained by constitutional subordination, identity-locked to provincial governance. Equalization-recipient provinces: d ≈ 0.4 (moderate beneficiary)—receive transfers but dependent on others' extraction, constrained exit. Separatist movements: excluded (not in the directionality calculus). The override mechanism was considered: none is necessary—structural derivation captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1867 external threat, infrastructure coordination) is dead—Canada faces no existential threat from fragmentation, and infrastructure coordination is subordinate to modern economic integration. Yet the constraint persists because federal actors benefit from continued subordination (resource taxation, policy authority) and legal/constitutional machinery (courts, amendment process) makes change impossible without federal consent. This is mandatrophy: the original justification is gone, but the constraint is maintained theatrically (equalization rhetoric, climate goals) and legally (courts affirming supremacy) rather than abandoned. The theater_ratio rise (0.25→0.42) tracks this: increasingly the constraint is justified by new rationales (climate policy, equalization) rather than the founding reason, a classic mandatrophy signal. The classification as tangled_rope (not snare) depends on the real coordination function existing; if that function degrades further, the engine would reclassify toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the Constitution Act 1867 establish a genuine federal hierarchy (supremacy) or a compact of sovereign provinces with enumerated powers?',
    'Textual analysis and constitutional history (what the framers intended) versus contemporary jurisprudence (how courts have interpreted it). The Secession Reference and related cases have settled the legal interpretation; historical scholarship documents the original framers intended a hierarchy, not a compact.',
    'If the text is ambiguous and compact-federalism has equal historical grounding, the federal supremacy claim loses constitutional inevitability and becomes a reading choice. If hierarchy was the original intent and remains the legal consensus, subordination is structurally entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, empirical, 'Whether the Constitution establishes federal supremacy or provincial compact.').

omega_variable(
    resource_sovereignty_separability,
    'Can provincial control of natural resources (s.92A Constitution Act 1982) ground a claim to absolute provincial sovereignty independent of the constitutional hierarchy?',
    'International law on indigenous and resource sovereignty; comparative federalism (how other systems treat resource ownership); political pressure and constitutional amendment attempts by resource-rich provinces.',
    'If resource ownership is foundational to sovereignty claims and cannot be overridden by enumerated federal powers, then provincial extraction targets could exit by asserting resource-based sovereignty. If federal trade authority prevails over provincial ownership, the constraint holds and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_separability, conceptual, 'Whether provincial resource ownership is separable from federal authority over interprovincial trade.').

omega_variable(
    identity_lock_persistence,
    'How deeply is provincial political identity fused with provincial government structures, and would that fusion break if exit became materially attractive?',
    'Post-exit cohort study (Quebec referenda, separatist movement mobilization) showing whether identity-lock persists after exit becomes politically salient. If elite and popular defection to federal identity occurs under sufficient economic pressure, identity-lock is contingent.',
    'If identity-lock is contingent and breaks under pressure, provinces'' exit_options upgrade from identity_locked to constrained (they become capable of negotiated exit). If identity-lock is structural/constitutional, the exit option remains trapped for the provincial state even if individuals defect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether provincial identity-lock is structural or contingent on material incentives.').

omega_variable(
    founding_problem_revival,
    'Could the founding problem (unified defense against external threat, transcontinental infrastructure coordination) revive under conditions of U.S. instability, climate migration, or resource conflict?',
    'Scenario analysis and historical precedent: did the founding problem ever truly disappear or was it only dormant? If U.S./geopolitical conditions change, would coordination justification revive?',
    'If the founding problem is structurally dormant but can revive, mandatrophy is temporary; if the problem is genuinely dead and the constraint persists only through legal and institutional inertia, mandatrophy is severe. Revival would reframe extraction as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_revival, preference, 'Whether the founding problem can revive or is permanently obsolete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t3, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(prov_tr_t3, observed).
narrative_ontology:measurement(prov_tr_t6, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(prov_tr_t6, observed).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(prov_tr_t10, observed).
narrative_ontology:measurement(prov_tr_t15, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(prov_tr_t15, observed).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(prov_tr_t20, observed).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(prov_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t3, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(prov_be_t3, observed).
narrative_ontology:measurement(prov_be_t6, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(prov_be_t6, observed).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(prov_be_t10, observed).
narrative_ontology:measurement(prov_be_t15, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(prov_be_t15, observed).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(prov_be_t20, observed).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(prov_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t3, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(prov_su_t3, observed).
narrative_ontology:measurement(prov_su_t6, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(prov_su_t6, observed).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(prov_su_t10, observed).
narrative_ontology:measurement(prov_su_t15, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(prov_su_t15, observed).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(prov_su_t20, observed).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(prov_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_transfer_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_climate_authority).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, interprovincial_trade_regulation).

% DUAL FORMULATION NOTE:
% provincial_sovereignty_boundary is a contested kernel with three structurally distinct readings: constitutional_subordination (this file) treats provinces as federal creatures with no inherent sovereignty; compact_federalism treats provinces as retaining residual sovereignty with exit negotiable; resource_sovereignty_primacy treats provincial resource ownership (s.92A) as grounding absolute territorial sovereignty. Each reading instantiates a different constraint with different beneficiary/victim structures and ε values. All three are linked as a constraint family via network.affects_constraints. The three readings do not represent different measurements of one constraint—they represent incommensurable readings of a contested institutional arrangement that different political coalitions advance. The engine computes per-seat classification for each reading separately; seat divergence across readings reveals how the ambiguity in the kernel enables different structural interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
