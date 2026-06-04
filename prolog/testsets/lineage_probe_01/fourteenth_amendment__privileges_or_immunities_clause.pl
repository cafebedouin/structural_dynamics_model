% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment__privileges_or_immunities_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment__privileges_or_immunities_clause, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fourteenth_amendment__privileges_or_immunities_clause
 *   human_readable: Privileges or Immunities Clause: Fourteenth Amendment's Stunted Engine
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The Privileges or Immunities Clause represents one hermeneutical path
 *   through the contested kernel of the Fourteenth Amendment. Enacted in 1868
 *   as the mechanism to establish national protection of substantive
 *   citizenship rights against state abridgment, the clause was understood by
 *   Reconstructionist Congress as grounding federal authority over state
 *   regulation of fundamental liberties. Five years later, the
 *   Slaughter-House Cases (1873) read the clause narrowly as protecting only
 *   a small set of rights inherent in national citizenship itself — primarily
 *   the right to interstate travel and the right to federal protection abroad
 *   — while leaving to states the regulation of occupations, public
 *   accommodations, and civil rights. This narrow reading hollowed the
 *   clause's intended function. The constraint documents how a textual
 *   mandate for national rights protection was judicially stunted into a dead
 *   letter, with the protective work migrating to Due Process Clause
 *   incorporation and Equal Protection doctrine. The clause persists in
 *   constitutional text and formal doctrine but performs minimal substantive
 *   work — its primary effect is state authority preservation, the opposite
 *   of its enactors' intent.
 *
 * KEY AGENTS:
 *   - Reconstructionist Congress (1868): Institutional actor (institutional/mobile at moment of enactment) — drafts and ratifies the clause as the engine of national rights protection; intends it as constraint on state authority
 *   - Individual Citizens and Freedmen (post-1873): Powerless agents (powerless/trapped) — the clause's intended beneficiaries; rendered without remedy by Slaughter-House narrowing
 *   - State Legislatures and Executives: Institutional beneficiaries (institutional/arbitrage) — benefit from the clause's narrow reading; preserve authority to regulate occupations, civil rights, public accommodations
 *   - The Judiciary: Institutional actor (institutional/arbitrage) — interprets the clause narrowly in Slaughter-House; maintains the doctrine through Piton-like application; later develops Due Process and Equal Protection as alternative protective mechanisms
 *   - Civil Rights Movement (20th century): Organized agent (organized/constrained) — seeks to revive or work around the clause's narrowness; employs Due Process and Equal Protection strategies instead; views clause as dormant recovery tool
 *   - Legal Academia and Constitutional Scholars: Analytical observers (analytical/analytical) — dispute the necessity of Slaughter-House; theorize alternative constructions of 'privileges or immunities'; debate whether clause revival is conceptually coherent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment__privileges_or_immunities_clause, 0.62).
domain_priors:suppression_score(fourteenth_amendment__privileges_or_immunities_clause, 0.78).
domain_priors:theater_ratio(fourteenth_amendment__privileges_or_immunities_clause, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment__privileges_or_immunities_clause, extractiveness, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment__privileges_or_immunities_clause, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment__privileges_or_immunities_clause, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment__privileges_or_immunities_clause, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment__privileges_or_immunities_clause, "Privileges or Immunities Clause: Fourteenth Amendment's Stunted Engine").
narrative_ontology:topic_domain(fourteenth_amendment__privileges_or_immunities_clause, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(fourteenth_amendment__privileges_or_immunities_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment__privileges_or_immunities_clause, '62c589fb-ed2d-42db-8838-2c2d8e728b2e').
narrative_ontology:cs_kernel_codification('62c589fb-ed2d-42db-8838-2c2d8e728b2e', fixed_text).
narrative_ontology:cs_authority_grounding('62c589fb-ed2d-42db-8838-2c2d8e728b2e', lineage).
narrative_ontology:cs_interpretation_layer_present('62c589fb-ed2d-42db-8838-2c2d8e728b2e').
narrative_ontology:cs_reading_relation('62c589fb-ed2d-42db-8838-2c2d8e728b2e', fourteenth_amendment__citizenship_clause, influences).
narrative_ontology:cs_reading_relation('62c589fb-ed2d-42db-8838-2c2d8e728b2e', fourteenth_amendment__due_process_clause, coexists_with).
narrative_ontology:cs_reading_relation('62c589fb-ed2d-42db-8838-2c2d8e728b2e', fourteenth_amendment__equal_protection_clause, influences).
narrative_ontology:cs_axiom('62c589fb-ed2d-42db-8838-2c2d8e728b2e', foundational, national_citizenship_substantive_rights).
narrative_ontology:cs_axiom_status(national_citizenship_substantive_rights, holdable).
narrative_ontology:cs_axiom_grounding('62c589fb-ed2d-42db-8838-2c2d8e728b2e', national_citizenship_substantive_rights, deontological).
narrative_ontology:cs_axiom('62c589fb-ed2d-42db-8838-2c2d8e728b2e', foundational, federal_enforcement_against_state_abridgment).
narrative_ontology:cs_axiom_status(federal_enforcement_against_state_abridgment, holdable).
narrative_ontology:cs_axiom_grounding('62c589fb-ed2d-42db-8838-2c2d8e728b2e', federal_enforcement_against_state_abridgment, instrumental).
narrative_ontology:cs_reference_frame('62c589fb-ed2d-42db-8838-2c2d8e728b2e', reconstructionist_national_citizenship_protection).
narrative_ontology:cs_drift_state('62c589fb-ed2d-42db-8838-2c2d8e728b2e', slaughter_house_narrowing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('62c589fb-ed2d-42db-8838-2c2d8e728b2e', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment__privileges_or_immunities_clause, fourteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment__privileges_or_immunities_clause, state_legislative_sovereignty).
narrative_ontology:constraint_victim(fourteenth_amendment__privileges_or_immunities_clause, national_citizenship_rights).
narrative_ontology:constraint_victim(fourteenth_amendment__privileges_or_immunities_clause, individual_plaintiffs_seeking_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CITIZEN (SNARE) — Cannot exit state jurisdiction; trapped in the state whose laws they seek protection against. The clause as judicially applied offers no protection — the citizen bears the full cost of state abridgment of rights the clause was meant to protect. The Slaughter-House hollowing means the clause provides zero functional remedy. Maximum experienced extraction with zero perceived recourse.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGISLATURE (ROPE) — Benefits from the clause's judicial hollowing. The Slaughter-House Cases permit states to abridge rights labeled as 'state citizenship' rather than 'national citizenship,' preserving legislative authority. The state experiences the constraint as pure coordination: the clause's narrow reading means states can regulate occupations, civil rights, and fundamental liberties with minimal federal override. Net beneficiary — the clause as applied coordinates state sovereign authority rather than restricting it.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CARPETBAGGER / FREEDMAN / DISFAVORED MINORITY (TANGLED ROPE) — Faces genuine state suppression (violence, deprivation of economic opportunity, exclusion from public accommodations) while the clause as applied provides no remedy. Constrained by geography, economic dependency, and violent suppression. Yet the clause exists — it was meant to protect them, and invocation of it creates some leverage (political cost of defying Reconstruction, formal commitment to the clause's text). The constraint is both extractive (state abridgment) and coordinating (the clause's existence establishes a legitimacy frame the state must at least gesture toward). Extraction is severe but not total because the clause's text and the Reconstruction settlement create some counterbalancing force.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY ADMINISTERING SLAUGHTER-HOUSE DOCTRINE (PITON) — The clause persists as formal constitutional text, but its functional application is degraded. Courts recite the Slaughter-House holding (the clause protects only rights of national citizenship, a narrow set) while rarely invoking it to strike down state laws. The clause has been largely displaced by Due Process Clause incorporation and Equal Protection doctrine, which provide the same individual rights protections through different pathways. The judiciary maintains the Slaughter-House reading through institutional inertia — the precedent stands, the doctrine is formally applied, but the real work of constitutional protection happens elsewhere. Theater ratio is high (0.65) because the clause's formal invocation obscures that it does little substantive work.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RECONSTRUCTIONIST CONGRESS (TANGLED ROPE, Historical) — At the moment of enactment, Congress intended the clause as the mechanism for federal protection of national citizenship rights against state abridgment. The clause coordinated this protective function — it solved the problem of how to establish national authority over state action without creating a unitary national government. But the clause also extracted from states by subordinating state sovereignty to national citizenship. Congress experienced the clause as balanced coordination with asymmetric federal gain. This perspective exists at the generational time horizon because Reconstruction itself was a generational moment; by the biographical horizon, the clause has already been hollowed. This perspective documents the original structural intent before judicial reinterpretation.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL RIGHTS MOVEMENT (SCAFFOLD) — Organized advocates see the clause as a dormant but recoverable legal tool. The clause's text is clear; Slaughter-House is a narrowing precedent that could be overruled or distinguished. Civil rights litigation strategy has periodically attempted to revive the clause as an independent basis for protecting substantive rights. This perspective sees the constraint as a temporary inert state with a potential sunset: if courts overrule Slaughter-House or construe the clause more expansively, the clause's protective function could be restored. The constraint is low in effective extraction because the organized agent has exit routes and sees the suppression as judicially imposed rather than textually inherent — hence reversible. Theater is moderate because litigation around the clause's scope involves real legal stakes even when the clause itself is currently dormant.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tension between national and state sovereignty over individual rights is an immutable feature of federal systems. The Privileges or Immunities Clause, like all clauses attempting to allocate rights protection between levels of government, necessarily encounters the logical contradiction: a federal government that enforces national rights against states must either subordinate state sovereignty (extraction from states) or create loopholes (suppression of individual rights). No constitutional text can resolve this contradiction. The Slaughter-House narrowing appears as an inevitable judicial retreat from an unsustainable structural claim. However, the structural data (identifiable beneficiaries in state legislatures, specific victims in freedmen and disfavored minorities, measurable suppression through specific state actions, measurable extraction through state authority preservation) contradicts the mountain classification. The engine's false summit detector will identify this as naturalization of a contingent doctrinal choice, not an immutable structural limit.
constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment__privileges_or_immunities_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment__privileges_or_immunities_clause, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment__privileges_or_immunities_clause, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourteenth_amendment__privileges_or_immunities_clause, TR),
    TR >= 0.70.

:- end_tests(fourteenth_amendment__privileges_or_immunities_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. At enactment (t0=0.15), the clause was read expansively by Reconstructionist proponents — it represented genuine national authority over state action to protect rights. By Slaughter-House (t8=0.58), the Court's narrow reading extracted a massive benefit for states: they regained authority to regulate the very domains (occupations, public accommodations, fundamental civil rights) that Reconstruction intended the clause to constrain. The extraction is not maximal (0.66+) because the clause's text remains, and the intended scope is still legible — the constraint is contingent on judicial interpretation, not structurally immutable. By post-incorporation (t20=0.62), extractiveness plateaus slightly as Due Process and Equal Protection provide parallel remedies, reducing the clause's relative neglect but not restoring its intended primacy. Suppression (0.78): High. The constraint operates through multiple suppression mechanisms: (1) judicial narrowing of the clause's scope, (2) state violence and legal deprivation of rights the clause was meant to protect, (3) exclusion of freedmen and disfavored minorities from access to remedies, (4) epistemic suppression (the clause is presented as naturally read narrowly, its broader historical reading erased from doctrine). Theater ratio (0.65): Moderate-high. The clause's continued presence in constitutional text and formal doctrine creates a theater of national rights protection, while the substantive work happens through Due Process and Equal Protection. Courts recite Slaughter-House language preserving the narrow reading while developing exceptions and alternative doctrines that accomplish the clause's intended work through different pathways. This displacement of the clause into secondary authority while maintaining its formal invocation is the defining feature of Piton dynamics in this constraint — institutional inertia preserving a doctrine that no longer does primary work.
 *
 * PERSPECTIVAL GAP:
 *   The Slaughter-House hollowing created a massive perspectival gap. State institutions experience the constraint as pure coordination or even beneficial restraint (Rope: the clause's narrow reading preserves their authority). Individual citizens experience it as pure extraction with no recourse (Snare: they lose the protection the clause was meant to provide, with no remedy). Reconstructionist Congress experienced it as balanced coordination with federal gain (Tangled Rope at enactment: the clause coordinated national protection AND extracted state authority). By Slaughter-House, the extraction side of the tangled rope severed — the coordination benefit to individuals vanished, leaving only extraction for states. The Civil Rights Movement perceives it as a temporary inert state with recovery potential (Scaffold: the clause text survives, precedent could be overruled, revival is possible). The Judiciary perceives it as a degraded doctrine that once mattered but now operates through alternative mechanisms (Piton: the clause persists through inertia, not function). The civilizational analytical observer risks seeing an immutable structural contradiction (Mountain: federal systems necessarily encounter federated rights allocation problems), but the structural data contradicts this — the hollowing is a specific, reversible doctrinal choice made by the 1873 Court, not a logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from the agent's structural position relative to this specific constraint. State legislatures are beneficiaries with arbitrage options (they can exit federal authority by invoking state sovereignty) — they get low d (approximately 0.15-0.20) and negative or low χ. Individual citizens and freedmen are victims with no exit (trapped by state jurisdiction and economic dependency) — they get high d (approximately 0.90-0.95) and high χ. The civil rights movement has constrained exit options (they can litigate and organize but face resource barriers and doctrinal constraint) — they get moderate-high d (approximately 0.65-0.75). The judiciary has arbitrage options (they can interpret the clause broadly or narrowly, choose among alternative doctrines) — they get moderate d reflecting their institutional authority balanced against internal legal constraints. The Reconstructionist Congress at the moment of enactment had institutional power and were promulgating the clause, so their d would be low (beneficiary perspective), but they perceived themselves as establishing a constraint on future state authority, not as extracting from states — their self-perception was pure coordination. The post hoc analytical observer sees the structural outcome: states gained authority preservation, individuals lost protection — suggesting they should have used higher d for state institutional actors or lower d for individual victims. The perspectival gap is structural: the beneficiary (states) and victims (individuals) experience radically different effective extractiveness from the same nominal constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how a single textual mandate (the Privileges or Immunities Clause) was subject to interpretive foreclosure by the Slaughter-House narrow reading. The mandatrophy does not dissolve because different perspectives genuinely see different constraint types — states see coordination (Rope), individuals see extraction (Snare), the clause's enforcers see degradation (Piton). The resolution is that the Slaughter-House interpretation is a contingent doctrinal choice that can be historicized: at the moment of enactment and ratification, the broader reading was live. By 1873, political and institutional forces had shifted, and the Court narrowed the clause. The mandatrophy is not semantic (all parties agree on what the clause says textually) but hermeneutical (parties disagree on what it constrains). The False Summit detection applies to the analytical mountain perspective: the notion that federal/state rights allocation is immutable is contradicted by the fact that Reconstructionist Congress believed it could mandate national protection and that modern civil rights litigation still views the clause as a potential lever. If the constraint were truly immutable, these actors would not perceive recovery possibilities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slaughter_house_necessity_vs_choice,
    'Was the Slaughter-House narrowing a logically necessary judicial interpretation, or a contingent historical choice that a different Court could have rejected?',
    'Doctrinal history comparing Slaughter-House reasoning to contemporary and later constitutional frameworks (Reconstruction Republican originalism, Civil Rights Act doctrine, modern substantive due process theory) and analysis of whether alternative interpretations of ''privileges or immunities'' were textually and historically available',
    'If necessary: the clause''s hollowing is inherent to constitutional structure (mountain from analytical view). If contingent: the clause''s hollowing is a reversible doctrinal choice, and the tangled_rope / snare classifications are diagnostically sound. This determines whether the constraint should be reclassified as a genuine mountain or remains a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slaughter_house_necessity_vs_choice, conceptual, 'Whether Slaughter-House narrowing was logically necessary or contingent doctrinal choice').

omega_variable(
    remedial_scope_ambiguity,
    'If a court were to revive the Privileges or Immunities Clause as an independent basis for individual rights protection, what scope of substantive rights would the clause be read to protect — only those essential to national citizenship, or the full range of fundamental rights now protected through Due Process and Equal Protection?',
    'Textual analysis of ''privileges or immunities of citizens'' across historical sources; comparison to scope of Due Process and Equal Protection protections in contemporary doctrine; hypothetical revival scenario analysis from civil rights litigators and constitutional scholars',
    'Narrow scope (only rights of national citizenship): clause remains weak, real protection remains displaced in Due Process (current state). Broad scope (fundamental rights): clause becomes robust alternative to Due Process, shifts remedial architecture, increases state suppression costs. Scope ambiguity explains why civil rights revival attempts have been cautious — the clause could be awakened into narrow form rather than expansive form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_scope_ambiguity, conceptual, 'The scope of substantive rights a revived Privileges or Immunities Clause would protect').

omega_variable(
    legitimate_vs_extractive_state_sovereignty,
    'Is the state sovereignty interest in limiting the Privileges or Immunities Clause (preserving state control over occupations, civil rights, fundamental liberties) a legitimate federalism principle or an extractive mechanism concealing state authority over disempowered groups?',
    'Historical analysis of state laws struck under competing doctrines (Due Process incorporation, Equal Protection); comparison of state laws that survived Slaughter-House narrowness vs those later struck under broader doctrines; correlation between Slaughter-House application and suppression of freedmen and disfavored minorities',
    'If legitimate: state sovereignty interest is genuine coordination value (supports rope classification for state institutional perspective). If extractive: state sovereignty is a cover story for state authority to abridge rights (supports snare/tangled_rope classification). This ambiguity explains the 0.78 suppression score — suppression operates partly through doctrinal hollowing (judicial mechanism) and partly through explicit state action (state mechanism), and they reinforce each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_vs_extractive_state_sovereignty, empirical, 'Whether state sovereignty interest in Slaughter-House narrowing is legitimate federalism or extractive mechanism').

omega_variable(
    reading_specificity_under_kernels_framework,
    'This reading instantiates the Privileges or Immunities Clause as one interpretation of the Fourteenth Amendment kernel. Does this reading foreclose the Due Process Clause reading, coexist with it, or influence it?',
    'Structural analysis of the two readings'' relationships: Do they claim mutually exclusive remedial pathways (foreclosure)? Do they operate simultaneously in contemporary doctrine (coexistence)? Does revival of the clause under this reading change the legitimacy or necessity of Due Process incorporation (influence)?',
    'Foreclosure: only one reading can be true, and revival of this reading would require abandoning Due Process incorporation (radical doctrinal shift). Coexistence: both readings are live in contemporary constitutional debate, with different parties advocating each. Influence: revival of this reading would make Due Process incorporation redundant or weaker, shifting the equilibrium without eliminating it. This omega documents the kernel contest explicitly within the constraint story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_under_kernels_framework, conceptual, 'Structural relationship between Privileges or Immunities and Due Process readings of Fourteenth Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment__privileges_or_immunities_clause, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poi_theater_t0_enactment, fourteenth_amendment__privileges_or_immunities_clause, theater_ratio, 0, 0.2).
narrative_ontology:measurement(poi_theater_t8_slaughter_house, fourteenth_amendment__privileges_or_immunities_clause, theater_ratio, 8, 0.6).
narrative_ontology:measurement(poi_theater_t20_post_incorporation, fourteenth_amendment__privileges_or_immunities_clause, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(poi_extractiveness_t0_enactment, fourteenth_amendment__privileges_or_immunities_clause, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(poi_extractiveness_t8_slaughter_house, fourteenth_amendment__privileges_or_immunities_clause, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(poi_extractiveness_t20_post_incorporation, fourteenth_amendment__privileges_or_immunities_clause, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(poi_suppression_t0_enactment, fourteenth_amendment__privileges_or_immunities_clause, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(poi_suppression_t8_slaughter_house, fourteenth_amendment__privileges_or_immunities_clause, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(poi_suppression_t20_post_incorporation, fourteenth_amendment__privileges_or_immunities_clause, suppression_requirement, 20, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment__privileges_or_immunities_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment__privileges_or_immunities_clause, fourteenth_amendment__due_process_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__privileges_or_immunities_clause, fourteenth_amendment__equal_protection_clause).
narrative_ontology:affects_constraint(fourteenth_amendment__privileges_or_immunities_clause, reconstruction_federal_authority_over_states).

% DUAL FORMULATION NOTE:
% The Privileges or Immunities Clause reading is upstream in the constraint family of Fourteenth Amendment readings. Due Process and Equal Protection readings are downstream — they developed partly as workarounds for the clause's hollowing, taking over the protective function Slaughter-House denied to the clause. The constraint family documents how a single amendment's intended structure (rights protection via national citizenship) was architecturally reorganized without textual change. Each reading of the Fourteenth Amendment kernel should be a separate constraint story, with the Privileges or Immunities reading documenting its specific extractiveness (0.62) and the supercession by alternative doctrines.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment__privileges_or_immunities_clause, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
