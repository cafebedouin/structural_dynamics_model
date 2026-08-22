% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Authorized Transformation of the Sacrifice Commitment: Prayer and Study as Instantiation
 *   domain: religious law / halakhic tradition / commitment systems
 *
 * SUMMARY:
 *   After 70 CE the sacrificial commandment complex became materially
 *   unperformable, and the rabbinic tradition effected what this reading
 *   holds to be an AUTHORIZED transformation: prayer and study constitute the
 *   commitment's present instantiation — not stopgaps for a suspended
 *   practice, but the practice itself in its current lawful form. The
 *   constraint examined here is that authorization-plus-enforcement
 *   arrangement: the standing post-Temple settlement in which the
 *   interpretive class defines what the ancient command now requires,
 *   communities live inside that definition, and dissent toward material
 *   performance is policed at the boundary. The claim/metrics split is
 *   deliberate and load-bearing: the claimed type is what I believe
 *   structurally true of the settlement (a genuine coordination achievement
 *   carrying real asymmetric extraction), while the metrics describe its
 *   actual operation as documented above — the engine computes per-seat
 *   classifications and adjudicates any divergence. Per the
 *   epsilon-invariance principle, the colloquial label 'what happened to the
 *   sacrifice commandment' decomposes into four structurally distinct claims
 *   (see network.dual_formulation_note); this file authors exactly one of
 *   them.
 *
 * KEY AGENTS:
 *   - - rabbinic_authority_structure: Agenda-setting beneficiary (institutional/arbitrage) — declares, administers, and enforces the transformed instantiation; collects interpretive authority, deference, and funding
 *   - - diaspora_jewish_communities: Coordinated beneficiary with payer residue (organized/constrained) — receives practicable covenant service; funds the structure and surrenders interpretive autonomy
 *   - - priestly_lineage_kohanim: Dispossessed payer (moderate/trapped) — hereditary sacrificial office reduced to honorific residue by the transfer of function to text and prayer
 *   - - material_performance_dissenters: Boundary-defining payer (moderate/trapped) — holds material performance non-negotiable; bears anathema, polemic, and exclusion; exit is schism
 *   - - restorationist_factions: Consolation-managed payer (moderate/identity_locked) — restoration hope reframed by the settlement as optional piety rather than outstanding obligation
 *   - - samaritan_gerizim_communities: Excluded living counterfactual (powerless/trapped) — maintains material sacrifice continuously outside the authorized conversation
 *   - - halakhic_historians: Analytical observer (analytical/analytical) — tests authorization claims against the documentary record from no seat's interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.57).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.45).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.57).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Authorized Transformation of the Sacrifice Commitment: Prayer and Study as Instantiation").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious law / halakhic tradition / commitment systems").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'a6b2a31e-9283-4be3-a126-40db8c59f7c8').
narrative_ontology:cs_kernel_codification('a6b2a31e-9283-4be3-a126-40db8c59f7c8', fixed_text).
narrative_ontology:cs_authority_grounding('a6b2a31e-9283-4be3-a126-40db8c59f7c8', lineage).
narrative_ontology:cs_interpretation_layer_present('a6b2a31e-9283-4be3-a126-40db8c59f7c8').
narrative_ontology:cs_reading_relation('a6b2a31e-9283-4be3-a126-40db8c59f7c8', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('a6b2a31e-9283-4be3-a126-40db8c59f7c8', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('a6b2a31e-9283-4be3-a126-40db8c59f7c8', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_axiom('a6b2a31e-9283-4be3-a126-40db8c59f7c8', foundational, binding_authority_may_transform_command_mode).
narrative_ontology:cs_axiom_status(binding_authority_may_transform_command_mode, holdable).
narrative_ontology:cs_axiom_grounding('a6b2a31e-9283-4be3-a126-40db8c59f7c8', binding_authority_may_transform_command_mode, theological).
narrative_ontology:cs_axiom('a6b2a31e-9283-4be3-a126-40db8c59f7c8', secondary, prayer_study_constitute_present_instantiation).
narrative_ontology:cs_axiom_status(prayer_study_constitute_present_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('a6b2a31e-9283-4be3-a126-40db8c59f7c8', prayer_study_constitute_present_instantiation, conventional).
narrative_ontology:cs_reference_frame('a6b2a31e-9283-4be3-a126-40db8c59f7c8', adaptable_sinaitic_commitment).
narrative_ontology:cs_drift_state('a6b2a31e-9283-4be3-a126-40db8c59f7c8', contemporary_restorationist_revival, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('a6b2a31e-9283-4be3-a126-40db8c59f7c8', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, priestly_lineage_kohanim).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_performance_dissenters).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, restorationist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares, administers, and enforces the transformed practice: fixes the liturgy that instantiates the old service, codifies the rules that govern it, and disciplines deviation through communal ban and polemic. Its interpretive authority is the asset the transformation created — the power to say what the command now requires — and it collects deference, institutional funding, and boundary-setting power directly.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, generational, arbitrage, global).

% Receives a practicable, portable form of covenantal service that requires no altar, no priesthood, and no central shrine — the practice that made communal and covenantal life sustainable across exile. It also funds the institutions that administer the practice and defers to authorized interpretation on questions its members cannot independently adjudicate; leaving the framework means leaving the covenantal community altogether.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, diaspora_jewish_communities, payer).

% Hereditary custodians of the sacrificial rite. The transformation transfers the material office they were born to into text and prayer administered by others; what remains to them is honorific residue — ceremonial precedence, the priestly blessing — without the function. Their identity is bound to a role the arrangement has hollowed out, and there is no path back to performance while the transformed settlement holds.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, priestly_lineage_kohanim, payer,
    moderate, generational, trapped, global).

% Hold that the offering command requires material performance and that study or prayer cannot discharge it. Inside the authorized framework their position is heresy: they face formal anathema, polemical refutation, and exclusion from communal institutions. Their realistic exit is schism — building separate communities outside the framework, at the cost of communal severance — which is the route their historical successors took.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_performance_dissenters, payer,
    moderate, generational, trapped, regional).

% Accept prayer and study but insist they are provisional — that the loss remains a live wound demanding rectification. The transformed settlement manages their grief consoling-ward: the liturgy remembers the Temple daily while the operative law treats the transformed practice as complete. Their hope is fused with covenantal identity itself, so exiting the framework would abandon the very covenant they seek to complete; staying means carrying a grievance the arrangement reframes as piety.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, restorationist_factions, payer,
    moderate, civilizational, identity_locked, global).

% Maintain material sacrifice on Mount Gerizim continuously from antiquity to the present. Their ongoing practice demonstrates that the material path remained physically possible somewhere — yet they stand wholly outside the authorized conversation, never granted standing in the deliberation that redefined the service. Their existence is the living counterfactual the framework does not engage.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, samaritan_gerizim_communities, excluded,
    powerless, generational, trapped, local).

% Trace the documentary record — Mishnah, Talmud, geonic responsa, liturgical accretion — against the authorization claims, asking whether the transformation's warrants precede the destruction or were constructed after it. They take testimony from every seat and owe allegiance to none of them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a portable, always-available instantiation of the covenantal service commitment after the Temple's destruction: a shared liturgical and textual discipline that coordinates dispersed communities onto one daily order of worship requiring no central shrine, no functioning priesthood, and no supply chain of offerings — solving the collective-action problem of maintaining covenant identity without territorial cult infrastructure.
% TRANSFER_FUNCTION: Moves interpretive authority and communal deference from the hereditary priesthood and the material rite to the rabbinic-interpretive class; moves atonement-access and covenantal standing from Temple-site performance to text-and-prayer participation routed through authorized interpretation; dissent from the transformation is paid for in communal standing — anathema, exclusion, or schism.
% ABSENT_VOICES: Material-performance voices (Samaritan, and later Karaite-style) sit structurally outside the authorized conversation — the Talmud grants no standing performance-only position after the destruction, and the anathema machinery met later dissent at the boundary. The priestly lineage speaks inside the framework mainly as honorific residue. Restoration-necessity advocates are heard in the liturgy daily, but their core premise — that the transformed practice is provisional — never entered operative law.
% DISAPPEARANCE_RATIONALE: If the transformation and its enforcement vanished overnight, the community would face the performance bind directly: a binding command materially unfulfillable for the diaspora, resolvable only by mass covenantal default or mass schism. The liturgical civilization constituted by the transformed practice — the siddur, the synagogue as surrogate sanctuary, the study academy as surrogate altar — would unravel, and the authority structure built on administering the transformation would lose its defining asset.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE) and the failed Bar Kokhba restoration (135 CE), the commanded sacrificial service became materially impossible: no altar, no functioning priestly rite, eventually no access to the site. The arrangement was built to solve how a command-bound community maintains covenantal continuity when the commanded act cannot be performed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Roman fiscal administration (the fiscus Judaicus) and Josephus attest the cultic rupture independently of rabbinic narrative; the Samaritan community's continuous Gerizim sacrifice and the later Karaite schism attest that the loss was experienced as a live catastrophe by parties with no stake in rabbinic authority's account; patristic literature registers the same cessation from outside the tradition entirely. Within the framework, the grief-stricken liturgy itself — fast days, consolation haftarot, the daily memorial of the offerings — corroborates that the founding problem was never treated as solved by forgetting.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.57 at interval end) is substantial but bounded: the settlement transfers interpretive supremacy, communal deference, and institutional support to the transforming class, and identifiable payers bear uncompensated losses — yet the service delivered (portable covenant worship sustaining a diaspora) is real, and the authorization claim carries deep traditional warrant rather than overnight fabrication. That combination — genuine coordination function plus named payers plus required enforcement — is why the claimed type is tangled_rope rather than rope or snare; the claim and the metrics were authored independently and the engine decides. Suppression (0.45) is authored as a raw structural property and enters the engine's computation unscaled; only extractiveness is scaled by directionality and scope. Note the suppression target: physical performance was rendered impossible externally (Roman destruction), so the enforcement machinery polices INTERPRETIVE alternatives — suspension-framings, material-necessity claims — not bodies. Theater (0.38) reflects the accretion of commemorative and mnemonic layers (daily recitation of the sacrificial orders, memorial fasts) atop a predominantly functional practice; the recitation's dual character — instantiation under this reading, archival rehearsal under the siblings — is precisely where the readings diverge. Accessibility collapse (0.65): within the framework, alternatives largely collapse once the transformation is accepted — unauthorized performance is incoherent and suspension-framing is rejected — but restoration-hope and cross-framework exit keep partial alternative space alive. Resistance (0.45): sustained historical resistance (the Karaite schism, Samaritan persistence, modern restorationism) bounded by anathema and small numbers; the dissenters' coalition power historically realized as exit rather than internal coalition, because internal coalition was exactly what the enforcement machinery priced out. All three tracked series share one eight-point grid (70/250/500/850/1250/1600/1900/2026). The suppression series traces a full enforcement cycle — build through codification, peak at the Karaite schism crisis (~850), decay under stabilization and emancipation, slight re-hardening amid contemporary restoration politics — driven by external crisis response, not intermittent reinforcement; extraction peaks with enforcement because boundary defense is when the settlement's asymmetry is most naked, then partially decays as the transformation naturalizes.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure and should compute differently. From the agenda-setting seat, the transformation is the completion of Sinai's own authority grant: the same chain that received the command governs its mode, and prayer-as-service is fidelity, not revision. From the kohanic seat it is dispossession dressed as elevation — a function they were born to, reassigned to others, with honors left as compensation. From the dissenter seat it is usurpation: a human hand redrawing a divine command and calling the redraw continuity, with anathema waiting for anyone who says otherwise. From the restorationist seat it is consolation that manages grief by redefining lack as fullness. The engine computes these divergences from power, exit, and role data; the gap between the agenda-setter's computed type and the payers' is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto directionality. rabbinic_authority_structure (beneficiary, arbitrage exit) derives d near the beneficiary pole — the settlement subsidizes its authority, and it can relocate its capital of legitimacy anywhere the community goes. diaspora_jewish_communities (beneficiary with payer residue, constrained exit) sits low but not at floor: genuine subsidy received, real deference paid. The three payer seats derive high d: kohanim and material_performance_dissenters are trapped (near full-target), restorationist_factions are identity_locked — the strongest target-side placement, since their grievance is fused with the identity the settlement administers. samaritan_gerizim_communities are excluded rather than coordinated; their exclusion is part of what the enforcement maintains, and they feed the structure as boundary-definition rather than as participants. Global scope applies the engine's modest verification-difficulty amplification. No directionality overrides were needed: beneficiary/victim declarations plus exit atoms produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — service without the material site — remains the community's daily operative condition, and the arrangement's function is exercised, not remembered; founding_problem_status=live paired with disappearance_verdict=world_rearranges yields no zombie flag under the mismatch consumer. The mandatrophy-relevant risk sits elsewhere and is tracked rather than asserted: theater_ratio climbs monotonically across the interval as mnemonic layers accrete, and the daily korbanot recitation is the hinge — instantiation under this reading, archival rehearsal under the siblings. If a restored Temple ever rendered the transformation unnecessary and the transformed practice persisted unchanged, the settlement would convert to vestige; omega restoration_conditionality carries exactly that contingency. The classification prevents mislabeling in both directions: reading the settlement as pure extraction (snare) erases the coordination achievement that carried a civilization through exile; reading it as pure coordination (rope) erases the dispossessed priest, the anathematized dissenter, and the managed mourner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_or_drift_status,
    'Is the transformation authorized — a legitimate exercise of binding authority over the command''s mode — or unauthorized drift: usurpation dressed as continuity?',
    'Comparative textual genealogy: test whether the transformation''s warrants (Hosea 14:3, the reported Yavnean rulings, the prayer-corresponds-to-sacrifice dicta) demonstrably precede the destruction or were constructed after it; weigh internal tradition-consistency against external documentary evidence (Roman, patristic, Samaritan) for continuity of authorization claims.',
    'This is the story''s master contingency. If drift, epsilon rises sharply, the victim set widens to every practitioner conscripted into a human redefinition of divine command, and the type trends snare — an authority structure extracting via control over what the command means. If authorized, the measured extraction reads as coordination cost plus boundary maintenance and the tangled_rope typing stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorization_or_drift_status, conceptual, 'Whether the transformation''s authorization is genuine or retrospective — the conditional that governs the expected structural delta.').

omega_variable(
    victim_set_composition,
    'Who actually bears unjust cost under the settlement: only the coerced dissenters, or also the dispossessed priesthood and the restorationists whose grief is consoling-managed rather than rectified?',
    'Comparative harm accounting across the sibling readings: each reading draws the victim boundary differently (performance_only counts every non-performing practitioner; this reading counts only the excluded and dispossessed), and the boundary determines which seats compute as targets.',
    'Victim-set breadth drives effective extraction for the payer seats and therefore the per-seat classifications; a narrow victim set supports the tangled_rope reading, a broad one pushes toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_composition, preference, 'Where the victim boundary falls — a values-dependent judgment the readings themselves contest.').

omega_variable(
    restoration_conditionality,
    'Is the transformed practice implicitly preparatory — valid only until a restored Temple dissolves it — or terminally complete, such that it would persist even alongside restored sacrifice?',
    'Counterfactual-theological analysis plus behavioral evidence: do transformation-reading authorities invest in restoration readiness, and does their liturgy treat restoration as completion of a gap or as addition to a finished practice? The daily korbanot recitation''s framed purpose (petition for restoration versus intrinsic instantiation) is the sharpest observable.',
    'If preparatory, the settlement carries an undeclared sunset and trends scaffold; if terminal, the transformation is genuine and the tangled_rope typing holds indefinitely. Resolves only if restoration ever occurs or the framework explicitly commits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_conditionality, conceptual, 'Hidden sunset question: whether the settlement''s justification is transition or steady state.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of material-performance dissent structural (anathema, exclusion from communal institutions, economic dependency on communal structures) or internalized (dissenters who accept the framework''s terms and police their own doubts)?',
    'Post-exit suppression trajectory: the Karaite exit is the natural experiment — if dissenters who left the framework ceased to exhibit the suppressed posture, suppression was structural; if restoration-anxiety and self-policing traveled with them, part of the suppression was internalized.',
    'If substantially internalized, the settlement''s effective suppression exceeds the structural measure — the framework reproduces compliance inside dissenters after exit — raising the target-side placement of dissenting seats; if structural, removal of the enforcement machinery would release the suppressed positions quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of dissent suppression, resolvable from schism trajectories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsym_read_tr_t70, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 70, 0.1).
narrative_ontology:measurement(tsym_read_tr_t250, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 250, 0.15).
narrative_ontology:measurement(tsym_read_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.2).
narrative_ontology:measurement(tsym_read_tr_t850, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 850, 0.24).
narrative_ontology:measurement(tsym_read_tr_t1250, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1250, 0.28).
narrative_ontology:measurement(tsym_read_tr_t1600, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(tsym_read_tr_t1900, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1900, 0.34).
narrative_ontology:measurement(tsym_read_tr_t2026, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(tsym_read_be_t70, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 70, 0.45).
narrative_ontology:measurement(tsym_read_be_t250, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 250, 0.5).
narrative_ontology:measurement(tsym_read_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(tsym_read_be_t850, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 850, 0.68).
narrative_ontology:measurement(tsym_read_be_t1250, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1250, 0.62).
narrative_ontology:measurement(tsym_read_be_t1600, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1600, 0.58).
narrative_ontology:measurement(tsym_read_be_t1900, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(tsym_read_be_t2026, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2026, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(tsym_read_su_t70, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 70, 0.3).
narrative_ontology:measurement(tsym_read_su_t250, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 250, 0.4).
narrative_ontology:measurement(tsym_read_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(tsym_read_su_t850, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 850, 0.72).
narrative_ontology:measurement(tsym_read_su_t1250, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1250, 0.6).
narrative_ontology:measurement(tsym_read_su_t1600, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1600, 0.52).
narrative_ontology:measurement(tsym_read_su_t1900, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement(tsym_read_su_t2026, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the fate of the sacrifice commandment after the Temple' covers four structurally distinct claims about one commitment's continuity status — authorized transformation (this file), study-as-performance-of-the-unchanged-command, material-performance-required, and suspended-pending-restoration. Each member authors its own epsilon over the same standing referent under its own lights; the members are linked here because the upstream authorization question (omega authorization_or_drift_status) is cited as evidence by every downstream reading, and contamination propagates along exactly these edges: if the authorization claim degrades, all four siblings' legitimacy conditions shift. This file's epsilon (0.57) differs from its siblings' by construction: performance_only assesses the same arrangement as two millennia of covenantal default (higher epsilon, different victim set), study_as_exercise assesses it as continuous performance (lower epsilon), hybrid_preparatory as incomplete suspension (intermediate, sunset-shaped).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
