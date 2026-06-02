% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta as Universal Due Process Constraint (1215 Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta (1215) is a contested kernel — a single persisting text that
 *   different parties read as instantiating fundamentally different
 *   constraints. This story models ONE reading: the universal rights
 *   interpretation that treats Clause 39 ('no freeman shall be imprisoned or
 *   punished except by the lawful judgment of his peers or by the law of the
 *   land') as establishing an inviolable due process right that applies to
 *   all persons within the jurisdiction, extending protection against
 *   arbitrary state power universally. Under this reading, 'free men' is
 *   expanded to mean all subjects, and Clause 39 emits a constraint on state
 *   detention and punishment authority that binds all rulers to procedure
 *   regardless of victim status. This reading transformed Magna Carta from a
 *   feudal agreement between king and barons into a transhistorical human
 *   rights precedent — a legitimacy claim for universal due process. However,
 *   the constraint's actual operation (as measured by base_extractiveness,
 *   suppression, and theater_ratio) reveals it as a tangled_rope: it
 *   coordinates legitimate state action through procedural requirements, but
 *   those same procedures become mechanisms for extracting compliance costs
 *   from the powerless, while the powerful retain discretion to evade through
 *   extrajudicial power. The measurements show declining extractiveness (0.68
 *   → 0.38) and rising theater (0.35 → 0.58) over 800 years: the constraint
 *   has become increasingly performative as written law has been formalized,
 *   yet increasingly effective at suppressing challenges to state power by
 *   channeling them into procedural forms the state controls.
 *
 * KEY AGENTS:
 *   - All Individual Subjects: Nominal victims of arbitrary state power (powerless/trapped) — universal due process constraint purports to protect this group but protection is conditional on enforcement capacity
 *   - Arbitrary State Power Apparatus: Institutional machinery of detention, punishment, confiscation (institutional/trapped) — the constraint restrains this apparatus's unconstrained operation
 *   - Common Freemen: Moderate-status agents (moderate/constrained) — can sometimes invoke procedural rights at cost and risk; depend on the constraint but cannot reliably enforce it
 *   - Magnates and High-Status Agents: Barons, bishops, merchants with resources to enforce rights (powerful/mobile) — experience constraint as pure coordination; can exit or resist
 *   - Rights-Claiming Movements: Organized coalitions demanding procedural protection (organized/constrained) — both benefit from constraint's legitimacy and bear extraction through state suppression of coordination
 *   - Analytical Observer: Transhistorical perspective (analytical/analytical) — risks treating contingent institutional arrangement as natural law or inevitable moral discovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.62).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta as Universal Due Process Constraint (1215 Reading)").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '8a3e4d05-c113-4bd4-91d7-58adaa11df60').
narrative_ontology:cs_kernel_codification('8a3e4d05-c113-4bd4-91d7-58adaa11df60', fixed_text).
narrative_ontology:cs_authority_grounding('8a3e4d05-c113-4bd4-91d7-58adaa11df60', lineage).
narrative_ontology:cs_interpretation_layer_present('8a3e4d05-c113-4bd4-91d7-58adaa11df60').
narrative_ontology:cs_reading_relation('8a3e4d05-c113-4bd4-91d7-58adaa11df60', baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('8a3e4d05-c113-4bd4-91d7-58adaa11df60', living_document_reading, coexists_with).
narrative_ontology:cs_axiom('8a3e4d05-c113-4bd4-91d7-58adaa11df60', foundational, all_persons_equal_procedural_right).
narrative_ontology:cs_axiom_status(all_persons_equal_procedural_right, holdable).
narrative_ontology:cs_axiom_grounding('8a3e4d05-c113-4bd4-91d7-58adaa11df60', all_persons_equal_procedural_right, deontological).
narrative_ontology:cs_axiom('8a3e4d05-c113-4bd4-91d7-58adaa11df60', foundational, clause_39_text_stable_universally_binding).
narrative_ontology:cs_axiom_status(clause_39_text_stable_universally_binding, holdable).
narrative_ontology:cs_axiom_grounding('8a3e4d05-c113-4bd4-91d7-58adaa11df60', clause_39_text_stable_universally_binding, conventional).
narrative_ontology:cs_reference_frame('8a3e4d05-c113-4bd4-91d7-58adaa11df60', universal_procedural_protection_for_all_subjects).
narrative_ontology:cs_drift_state('8a3e4d05-c113-4bd4-91d7-58adaa11df60', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8a3e4d05-c113-4bd4-91d7-58adaa11df60', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_individual_subjects).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, claim_to_procedural_protection).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, unconstrained_state_power).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, arbitrary_detention_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT WITHOUT PROTECTION (SNARE) — An individual facing state power without access to due process protection, or denied access through state evasion of procedural constraints. Trapped by jurisdiction with no exit option. Experiences maximum extraction: vulnerability to arbitrary detention, punishment without trial, confiscation without law. The constraint's existence creates a nominal right but the subject's powerlessness means the state can suppress knowledge of the right itself.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMON FREEMAN (TANGLED ROPE) — A person of some property or status (merchant, craftsman, minor landholder) who can invoke procedural protections at significant cost and risk, but also depends on those protections to maintain their modest position. Constrained by the cost of litigation and by fear of state retaliation for invoking rights. Experiences both genuine coordination (the right structures legitimate state action) and real extraction (the state retains discretion to oppress through procedural evasion or extrajudicial power). Mixed experience — can exercise the right sometimes, but not reliably.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAGNATE OR HIGH-STATUS AGENT (ROPE) — A baron, bishop, or wealthy merchant with resources to enforce rights, alternative jurisdictions to flee to, and networks to resist state overreach. Mobile because they can exit the constraint or relocate. Experiences the constraint purely as coordination: it structures legitimate exercise of power and protects their own substantial interests. Clause 39 serves them as a coordination mechanism — it prevents arbitrary seizure of their lands or imprisonment without law.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ARBITRARY STATE POWER APPARATUS (SNARE) — The institutional machinery of unconstrained detention, confiscation, and punishment. This perspective sees the constraint as a victim — it is being restrained by Clause 39. Trapped by the law that forbids it from operating according to its own internal logic (maximum efficiency through force without procedure). The apparatus experiences the constraint as extraction of its freedom of action. From this inverted perspective, the constraint is a snare on state capacity.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHTS-CLAIMING MOVEMENT (TANGLED ROPE) — Organized agents (barons, clergy, commons claiming rights) who both benefit from the constraint's existence (it legitimates their resistance to arbitrary power) and bear extraction (the state retains mechanisms to suppress their organization or ignore the constraint). Constrained by the state's ability to revert to extrajudicial power. The movement experiences the constraint as both protection and limitation — it provides a focal point for coordination but also imposes a procedural requirement that slows resistance when faster action is needed.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a transhistorical, universal perspective, the constraint appears as an immutable axiom: human dignity and the right to trial before punishment are self-evident truths that no legitimate authority can override. This perspective naturalizes the constraint as a discovery of pre-existing law rather than a constructed limitation. However, the structural data (require_active_enforcement=true, beneficiaries and victims identified, extractiveness > 0.25) contradicts the mountain classification. This is a false summit — the 'universal human right' framing naturalizes what is actually a contested institutional arrangement that requires enforcement and benefits specific agents.
constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__universal_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Under the universal-rights reading, the constraint purports to protect all subjects from arbitrary detention and punishment. However, actual enforcement depends on access to legal process, resources for litigation, and organized power to resist state evasion. The moderate value reflects that the constraint provides nominal protection (reduces worst-case extraction scenarios) but leaves substantial discretion for state power to extract through procedural mechanisms, extrajudicial channels, or selective enforcement. The measurement trajectory (declining from 0.68 at t=0, when the constraint was new and rarely enforced, to 0.38 at t=400, when procedural enforcement mechanisms matured) shows that proceduralization reduced bare extraction but increased theater and embedded suppression through law itself. Suppression (0.62): Moderate-high. Multiple barriers prevent effective invocation: cost of legal process, fear of state retaliation for claiming rights, lack of knowledge of rights, and state capacity to operate outside procedure through emergency powers, extrajudicial authority, or simple non-compliance. However, suppression is not total — magnates and organized groups can invoke the constraint. Theater ratio (0.58): Moderate-high, rising over time. Historical interpretation required extensive performative work: invocation of the constraint's legitimacy, ceremonial re-issuance, coronation oaths, parliamentary recitation. The theater rose as the constraint became more distant from original context and required more interpretive work to extend to new situations (from feudal disputes between king and barons to modern criminal procedure). The rising trajectory reflects that the constraint's force increasingly depends on ritual invocation rather than structural enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximum perspectival gap. The powerless subject sees a snare (nominal protection but suppressed enforcement). The magnate sees rope (pure coordination protecting their interests). The common freeman sees tangled rope (mixed protection and extraction). The arbitrary state apparatus sees constraint as snare on itself. The rights-claiming movement sees tangled rope (legitimacy with limits). The analytical observer risks seeing mountain (universal natural law) but structural data reveals false summit (contingent institutional arrangement with identifiable beneficiaries and enforcement mechanisms). The gap arises because the universal-rights reading posits a single constraint that operates identically for all persons, but actual enforcement depends entirely on agent power, exit options, and resource access. The constraint's universality is nominal — its effects are stratified by power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural relationship to due process protection. Powerless subjects (d ≈ 0.95, trapped + victim status) experience maximum extraction from suppression mechanisms. Magnates (d ≈ 0.15, mobile + beneficiary status) experience low effective extraction because they can enforce the constraint. Common freemen (d ≈ 0.65, constrained + mixed victim/beneficiary status) experience moderate extraction. The arbitrary state apparatus is itself a 'victim' of the constraint — from its perspective, d reverses (it experiences suppression of its own power). This unusual directionality (treating institutional apparatus as trapped victim) is semantically inverted but structurally accurate: the constraint binds the state just as it binds individual agents. No beneficiary/victim override needed — the structural derivation is correct.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (contradiction between Clause 39's claim to protect all from arbitrary power and its actual operation as selective coordination for the powerful) is resolved by recognizing that this reading instantiates a false summit: it naturalizes a contingent institutional arrangement as universal human right. The constraint is not mountain (immutable natural law), nor is it pure rope (pure coordination without asymmetric extraction). It is tangled_rope because it serves genuine coordination function (all parties benefit from rule of law in principle) AND it produces asymmetric extraction (enforcement depends entirely on power to invoke rights). The mandatrophy dissolves when we recognize that universality claims about rights can coexist with stratified enforcement — the constraint is universal in scope but particularistic in implementation. This is not a contradiction; it is the constraint's actual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_scope_ambiguity,
    'Does ''free men'' in Clause 39 (1215) mean literally all persons, or was it a reading imposed retroactively that transforms a baronial privilege agreement into a universal rights document?',
    'Historical textual analysis of 1215 drafting intent vs. subsequent 13th-16th century interpretations; comparison of contemporaneous usage of ''free men'' in legal documents; examination of which groups actually invoked Clause 39 across centuries.',
    'If universal scope was original intent: this reading is structurally straightforward — the constraint extends to all subjects from inception. If retroactive expansion: the universal reading is a reframing strategy that redefines the kernel itself, not merely one interpretation of it. Changes whether this is a reading of a kernel or the kernel has been reconstructed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_scope_ambiguity, empirical, 'Whether universal scope was original 1215 intent or later retroactive imposition').

omega_variable(
    enforcement_mechanism_sufficiency,
    'Does the written constraint (Clause 39 text alone) provide sufficient enforcement mechanism to bind state power, or does its force depend entirely on organized resistance by those powerful enough to enforce it?',
    'Historical case studies of successful invocation vs. state evasion; analysis of which groups actually secured remedies for violations; examination of enforcement instruments (habeas corpus development, judicial review, executive constraint mechanisms).',
    'If text alone is binding: the constraint is more rope-like than analysis suggests — it coordinates through shared acceptance of law. If enforcement depends on organized power: the constraint is more tangled_rope/snare — those without power to enforce it experience suppression. This determines whether the constraint functions as stated or whether it requires organized enforcement to be real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sufficiency, empirical, 'Whether written text or organized enforcement power is primary constraint mechanism').

omega_variable(
    arbitrage_closure_credibility,
    'Can a subject credibly ''exit'' the jurisdiction to avoid arbitrary state power, or is the constraint''s suppression value artificially high because practical exit is impossible for most historical populations?',
    'Historical mobility analysis: what fraction of population could actually relocate or claim asylum elsewhere? Which groups actually used exit as escape from arbitrary power vs. which were trapped? Geographic and legal barriers to exit in different periods.',
    'If exit was genuinely unavailable for most: suppression measure understates the binding force — the constraint''s lack of effectiveness is worse because exit is impossible. If exit was available for some: the constraint''s effectiveness depends on agent power (high-status agents use exit, low-status cannot). This refines the directionality model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrage_closure_credibility, empirical, 'Whether exit/arbitrage was credible option across social strata').

omega_variable(
    reading_vs_kernel_boundary,
    'Is this story a reading of a contested kernel (Magna Carta''s meaning remains disputed), or is the universal-rights reading actually a reconstruction/replacement of the kernel rather than a reading of it?',
    'Definitional: if this reading coexists with rival readings held by different parties simultaneously (siblings exist, are live), it is a reading. If this reading has foreclosed its siblings within the contemporary legal tradition, it may have become THE kernel rather than a reading.',
    'If reading: the sibling readings (baronial_privilege, living_document) are live and coexist. The framework remains contested. If the universal reading has become hegemonic: it has become the canonical form and siblings are relegated to historical interest only. This affects whether cs_structure.reading_relations should use ''coexists_with'' or ''forecloses''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_kernel_boundary, conceptual, 'Whether this is a reading of contested kernel or has become the canonical kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_univ_theater_t0, magna_carta_1215__universal_rights_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mc_univ_theater_t200, magna_carta_1215__universal_rights_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(mc_univ_theater_t400, magna_carta_1215__universal_rights_reading, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(mc_univ_extract_t0, magna_carta_1215__universal_rights_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(mc_univ_extract_t200, magna_carta_1215__universal_rights_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(mc_univ_extract_t400, magna_carta_1215__universal_rights_reading, base_extractiveness, 400, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(mc_univ_suppress_t0, magna_carta_1215__universal_rights_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(mc_univ_suppress_t200, magna_carta_1215__universal_rights_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(mc_univ_suppress_t400, magna_carta_1215__universal_rights_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_writ_limitation).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, trial_by_jury_procedural_requirement).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, executive_detention_authority_constraint).

% DUAL FORMULATION NOTE:
% Magna Carta decomposes into multiple constraint stories along epistemic boundaries. The universal-rights reading (this story) treats the constraint as establishing universal due process. The baronial-privilege reading (sibling) treats it as feudal property protection. The living-document reading (sibling) treats it as evolving constitutional authority. These are NOT the same constraint viewed from different angles — they have materially different epsilon values (universal reading ε=0.38, baronial reading ε≈0.15, living-document reading ε≈0.45). The network links the stories across the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
