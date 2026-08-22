% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__modernization_reading, []).

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
 *   constraint_id: orthographic_kernel__modernization_reading
 *   human_readable: Latin Script Orthographic Transition (Modernization Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   The Turkish state implements mandatory Latin-script orthography to enable
 *   technical modernization and rapid literacy expansion. The reading authors
 *   this constraint as a genuine coordination achievement (script
 *   standardization + Western technical access) that carries asymmetric costs
 *   (devaluation of Arabic-script expertise, displacement of older literates,
 *   institutional rupture with Islamic scholarship traditions). This reading
 *   claims the constraint is tangled_rope: real coordination function
 *   (modernization, technical access), real extraction (expertise
 *   devaluation, generational displacement), active enforcement (prohibition
 *   of Arabic-script official use), beneficiaries (state, new educated
 *   cohort), and victims (displaced administrators, older literates, excluded
 *   ulema). The founding problem (fragmented scripts, slow technical
 *   adoption) is real but contested: some accounts argue the problem could be
 *   solved through pluralism rather than replacement. Theater moderates over
 *   time as the initial pedagogical efforts (genuine re-education work)
 *   settle into administrative routines; enforcement remains steady.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: institutional agenda-setter; drives and enforces transition; collects authority and control over literacy standards
 *   - new_literate_class: moderate-power beneficiary; gains rapid literacy and technical access without costly multi-year script mastery
 *   - arabic_script_administrators: moderate-power victims; expertise devalued; constrained re-training or displacement
 *   - ulema_knowledge_guardians: powerful but excluded; identity-locked; authority rests on Arabic script preservation; face systematic institutional devaluation
 *   - older_generation_literates: powerless victims; trapped in functional illiteracy; no re-training pathway available
 *   - western_technical_community: institutional beneficiary; rapid technology transfer, infrastructure alignment
 *   - islamic_transnational_networks: excluded; lose interpretive partners in Turkish scholarly networks once script dominates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.52).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.68).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "Latin Script Orthographic Transition (Modernization Reading)").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, 'aceb357a-abd0-4a22-8e6b-9502e3508395').
narrative_ontology:cs_kernel_codification('aceb357a-abd0-4a22-8e6b-9502e3508395', fixed_text).
narrative_ontology:cs_authority_grounding('aceb357a-abd0-4a22-8e6b-9502e3508395', extraction).
narrative_ontology:cs_interpretation_layer_present('aceb357a-abd0-4a22-8e6b-9502e3508395').
narrative_ontology:cs_reading_relation('aceb357a-abd0-4a22-8e6b-9502e3508395', orthographic_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('aceb357a-abd0-4a22-8e6b-9502e3508395', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('aceb357a-abd0-4a22-8e6b-9502e3508395', foundational, script_adoption_enables_technical_progress).
narrative_ontology:cs_axiom_status(script_adoption_enables_technical_progress, holdable).
narrative_ontology:cs_axiom_grounding('aceb357a-abd0-4a22-8e6b-9502e3508395', script_adoption_enables_technical_progress, instrumental).
narrative_ontology:cs_axiom('aceb357a-abd0-4a22-8e6b-9502e3508395', foundational, turkish_linguistic_identity_survives_script_change).
narrative_ontology:cs_axiom_status(turkish_linguistic_identity_survives_script_change, overridden).
narrative_ontology:cs_axiom_grounding('aceb357a-abd0-4a22-8e6b-9502e3508395', turkish_linguistic_identity_survives_script_change, deontological).
narrative_ontology:cs_reference_frame('aceb357a-abd0-4a22-8e6b-9502e3508395', technical_modernization_through_script_standardization).
narrative_ontology:cs_drift_state('aceb357a-abd0-4a22-8e6b-9502e3508395', post_generation_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aceb357a-abd0-4a22-8e6b-9502e3508395', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, technical_educators).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, arabic_script_administrators).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, ulema_knowledge_guardians).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, older_generation_literates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, western_technical_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drives the Latin script reform as modernization strategy to centralize control, synchronize with Western technical standards, and enable rapid training of new administrative cadres. Funds the transition infrastructure (schools, printing, official documents), enforces script standardization across all state communication, and controls the narrative that Latin script is merely an enabling technology for progress, not a cultural break.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, state_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).

% Youth educated in state schools learn Latin script as the sole writing system; they gain access to Western scientific and technical literature, modern printing, and bureaucratic careers without the multi-year commitment to Arabic script mastery. Their literacy expands rapidly under the transition, making them structurally advantaged relative to the prior Arabic-script-educated generation. No barrier to entry; the state provides the educational apparatus.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_class, beneficiary,
    moderate, biographical, mobile, national).

% Educators trained in or influenced by Western technical traditions benefit from the script transition: their curriculum becomes the state-mandated standard, their pedagogical approaches are legitimized, their authority grows. Technical education becomes prestigious and state-funded. They operate the new literacy infrastructure.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, technical_educators, beneficiary,
    moderate, biographical, mobile, national).

% Officials and scribes trained in Ottoman Arabic-script bureaucracy find their expertise devalued overnight. Transition to Latin script requires re-training or displacement; older administrators may be left behind, their career-long expertise rendered obsolete by policy choice. Those who attempt transition face a steep learning curve competing with younger cohorts already fluent in Latin script.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, arabic_script_administrators, payer,
    moderate, biographical, constrained, national).

% Religious scholars whose authority rests on mastery of Arabic-script Quranic and hadith traditions face systematic devaluation of that expertise. Latin-script printing of religious texts (or refusal to print them) alters the transmission chain; ulema are excluded from the script-transition decision but their institutional authority depends on script preservation. Identity fusion (religious authority inseparable from textual tradition) makes exit impossible.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ulema_knowledge_guardians, payer,
    powerful, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, ulema_knowledge_guardians, excluded).

% Adults who achieved literacy in Arabic script under the Ottoman system cannot re-learn new script in adulthood; they become functionally illiterate in the new system overnight. No alternative pathway exists; the script transition is total and mandatory for all official communication. Age makes re-education infeasible; they are trapped.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, older_generation_literates, payer,
    powerless, biographical, trapped, national).

% Western nations and their technical networks benefit from rapid technical transfer into Turkish contexts (printing, telecommunications, scientific publishing) once Latin-script infrastructure exists. The constraint aligns Turkish institutional capacity with Western technological standards, reducing adaptation costs for foreign investment and technical collaboration. They provide models, machinery, and legitimating framing for the transition.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, western_technical_community, beneficiary,
    institutional, generational, arbitrage, global).

% Broader Islamic scholarly and religious networks that depend on Arabic-script transmission of authoritative texts are severed from Turkish religious knowledge production once Latin script dominates. They lose interpretive partners; Turkish ulema cannot participate in transnational Islamic scholarship using the new script. Their exclusion from the decision is structural — script choice sits entirely within Turkish state authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, islamic_transnational_networks, excluded,
    organized, civilizational, trapped, global).

% Formal legislative body that approves and codifies the script reform. Takes testimony from state bureaucrats (urging transition for modernization), technical educators (claiming efficiency gains), and some religious voices (opposing cultural rupture). Makes the binding decision to enforce Latin script universally, then oversees implementation and resistance management.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, national_assembly, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns Turkish written communication with Latin-script-based international technical and scientific standards, enabling rapid technology transfer and reducing the cost of adopting foreign machinery, printing, telecommunications, and scientific texts. Creates a single, state-standardized writing system replacing the regional/institutional script variation of the late Ottoman period.
% TRANSFER_FUNCTION: Transfers literacy-based social advantage from the Arabic-script-educated generation (administrators, ulema, older literates) to youth educated in state Latin-script schools. Transfers devalued expertise from Arabic-script bureaucratic and religious specialists to new Latin-script administrators and technical educators. Transfers regulatory authority from Ottoman Islamic knowledge guardians (ulema) to state educational apparatus.
% ABSENT_VOICES: Ulema and Islamic transnational networks are structurally excluded from the script decision; they would attest that the constraint severs living tradition, breaks the textual chain of Islamic scholarship, and subordinates religious authority to state secular authority. Older-generation literates have no institutional voice — their displacement is uncontested in formal proceedings. Ottoman Arabic-script administrators lobby for gradual transition but are overruled by state modernization priority.
% DISAPPEARANCE_RATIONALE: If Latin-script enforcement vanished, Ottoman officials would not re-establish Arabic script universally (the institutional apparatus of Ottoman governance is gone), but a dual-script literacy system would emerge: Latin script for technical and state communication, Arabic script preserved for religious and cultural domains. The ulema would restore Arabic-script printing and education. The older generation would retain their literacy in a living multilingual ecology instead of becoming illiterate. The constraint's absence forces immediate institutional reorganization.
% FOUNDING_PROBLEM: Ottoman Turkish written communication was fragmented across Ottoman cursive, Arabic script, and Persian script conventions; technical and scientific literature was primarily in European languages or classical Arabic; the Ottoman administration could not rapidly train new cadres of literate officials or absorb Western machinery documentation without costly translation and script-bridging overhead.
% FOUNDING_PROBLEM_CORROBORATION: State bureaucrats and technical educators attest the founding problem is still live: technical modernization requires script alignment with Western standards and rapid literacy expansion. Ulema and Islamic scholars attest the founding problem conflates technical accessibility with cultural identity, and that the problem could be solved through trilingual education and script pluralism without script replacement. Historical analyses outside the state apparatus document that script fragmentation was manageable and that the transition was politically motivated as much as technically necessary.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__modernization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__modernization_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__modernization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.35 (technical access genuine, costs not yet manifest) and rises to 0.52 as displacement cascades and the beneficiary/victim asymmetry stabilizes. Suppression rises more steeply (0.42→0.68) because enforcement machinery must actively prohibit Arabic-script use in official contexts; this is not merely natural friction but deliberate barrier maintenance (regulatory enforcement of script purity). Theater tracks enforcement: early phase includes genuine pedagogical work (25% theater), but by year 10+ enforcement becomes routine script-compliance checking with diminishing educational content (41% theater by year 25—administrative activity performing modernization more than executing it). The constraint is tangled_rope by structure: real coordination (script standardization enables technical transfer), real extraction (expertise devalued, older generation displaced), active enforcement (state authority maintains Latin-script monopoly), and asymmetric beneficiary/victim sets. One shared time grid: every metric authored at every point (0, 3, 6, 10, 15, 20, 25) ensures the engine samples all metrics on the same temporal surface.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state bureaucracy) perceives the constraint as genuine Rope: technical modernization, rational standardization, expanding literacy opportunity. The constrained payer seats (displaced administrators, trapped older literates) perceive Snare: expertise destruction, enforced loss of competence, no exit. The excluded powerful seat (ulema) perceives an extraction mechanism disguised as modernization: the state leverages technical necessity to displace religious authority. These divergent perceptions emerge from the same structural facts—state control, script enforcement, beneficiary selection, victim cost—but from seats with radically different exit options and power positions. The engine computes per-seat classification from the authored structural data; this perspectival gap is where the divergence lives.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy is the structural beneficiary: it controls literacy standards, shapes educational apparatus, acquires regulatory authority over script use, and benefits from reduced coordination costs with Western technical systems (d near 0.15—full beneficiary). New literate class benefits from state-provided education and technical access (d near 0.20). Arabic-script administrators are constrained victims: their expertise is devalued, re-training is costly and competitive, their career trajectory is broken (d near 0.75). Ulema are powerful but extracted through institutional devaluation: their identity is fused with script (identity_locked exit), their authority depends on script preservation, and they are excluded from the decision (d near 0.80—full target, despite power atom). Older-generation literates are trapped victims, functionally illiterate in the new system with no exit pathway (d near 0.90—maximum target). Western technical community derives arbitrage benefit without bearing enforcement cost (d near 0.05). The directionality derivation produces expected seat divergence: a moderate bureaucrat sees coordination + mild efficiency cost (d=0.3→low extraction per seat); a trapped older literate sees only displacement and loss (d=0.9→high extraction per seat). No overrides needed; the structural data drives the divergence cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy test: Does the constraint's mandate (technical modernization, literacy expansion) persist beyond its founding problem? The founding problem (fragmented scripts, slow technical adoption) is substantially resolved by year 10: Latin-script schools are established, youth literacy expands, technical literature flows in. By year 20, the educational infrastructure is mature, technical modernization is proceeding, and the founding problem is manifestly solved. Yet the constraint's enforcement machinery remains at full strength (suppression_requirement stable at 0.68, theater_ratio stable at 0.41): the script-purity rules persist, Arabic-script use remains prohibited, and no relaxation occurs. The constraint has become mandatrophic—its original mandate is dead (technical modernization is achieved independent of enforcing Latin-script monopoly; pluralistic scripts could coexist with ongoing technical progress), but the constraint persists due to institutional inertia and state interest in script standardization. This is NOT Piton (theater is moderate, not theatrical), but it is a Tangled Rope exhibiting mandatrophy: the extraction function (state authority, institutional control) has decoupled from the coordination function (technical modernization), yet enforcement continues. The measurement trajectory documents this: extractiveness plateaus at 0.52 while suppression plateaus at 0.68, indicating the system is no longer doing the coordination work but is performing enforcement work. Theater at 0.41 is not piton-grade (>0.5), so this is not theatrical persistence—it is functional extraction riding on a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_necessity_vs_political_choice,
    'Is Latin-script adoption a technical necessity for modernization, or a political choice that leverages modernization as legitimation?',
    'Counterfactual institutional design: what would Turkish technical modernization look like under trilingual (Arabic/Persian/Latin) literacy policy with state funding for all three scripts? What are the actual marginal costs of maintaining script pluralism relative to enforcing script monopoly?',
    'If technical modernization is achievable under pluralism with modest overhead, the constraint is political extraction disguised as modernization (higher ε, reclassifies toward Snare). If pluralism carries severe technical barriers, the modernization framing is accurate (ε stable as tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_political_choice, empirical, 'Whether script enforcement is technically necessary or politically chosen.').

omega_variable(
    founding_problem_obsolescence,
    'Once technical modernization is achieved and new-cohort literacy is established, does the founding problem persist or does the constraint become mandatrophic?',
    'Time-series measurement of whether script-purity enforcement continues AFTER the founding problem is resolved. If enforcement persists at full strength after technical modernization is complete, the constraint has become extraction divorced from its coordination mandate.',
    'If mandatrophy is detected (stable enforcement + dead founding problem), the constraint reclassifies from tangled_rope toward snare-with-institutional-drag: extraction is the primary function, modernization was the cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s original mandate persists or enforcement outlives functional necessity.').

omega_variable(
    identity_lock_reversal,
    'Is the ulema''s identity-lock to Arabic script irreversible, or does a new generation of Islamic scholars emerge who operate fluently across Latin and Arabic scripts?',
    'Longitudinal observation of whether younger ulema accept Latin-script religious publishing or whether religious textual authority remains Arabic-script-bound across generational transition.',
    'If identity-lock persists (younger ulema refuse Latin-script engagement), the ulema remain trapped targets. If identity-lock breaks (multilingual religious scholars emerge), the constraint''s extraction from the excluded-powerful seat diminishes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversal, empirical, 'Whether religious-textual authority can detach from Arabic script or remains identity-fused.').

omega_variable(
    older_generation_literacy_trajectory,
    'Do older-generation Arabic-script literates become permanently functionally illiterate, or do they maintain residual reading capacity and cultural transmission?',
    'Post-transition literacy surveys: what proportion of older adults retain Arabic-script reading ability? Do they transmit it informally to younger generations despite script-enforcement prohibition?',
    'If permanent functional illiteracy (suppression mechanism fully internalized), the constraint operates as pure internalized cognitive suppression after the first few years. If residual literacy and informal transmission persist (suppression only structural), the constraint requires ongoing enforcement to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(older_generation_literacy_trajectory, empirical, 'Whether suppression of older-generation literacy is structural or internalized/permanent.').

omega_variable(
    reading_boundary_oracle,
    'Is this reading (modernization framing) distinguishable from the rupture_reading based on the data, or do they generate empirically identical constraint structures?',
    'Examine the state''s actual justifications for script enforcement and official narratives. If state rhetoric emphasizes technical modernization and linguistic continuity (''preserving Turkish identity''), the modernization reading holds. If rhetoric emphasizes cultural rupture and deliberate break with Ottoman/Islamic past (''creating new national identity''), the rupture reading is the correct framing.',
    'If the readings collapse into empirical indistinguishability, the committer-axis distinction is conceptual only and does not drive structural divergence. The three readings remain live but one may dominate empirically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_oracle, conceptual, 'Whether the modernization and rupture readings produce different empirical constraint structures or are axiomatically identical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__modernization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(orth_tr_t3, orthographic_kernel__modernization_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(orth_tr_t6, orthographic_kernel__modernization_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__modernization_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(orth_tr_t15, orthographic_kernel__modernization_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__modernization_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(orth_tr_t25, orthographic_kernel__modernization_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__modernization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orth_be_t3, orthographic_kernel__modernization_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(orth_be_t6, orthographic_kernel__modernization_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__modernization_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(orth_be_t15, orthographic_kernel__modernization_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__modernization_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(orth_be_t25, orthographic_kernel__modernization_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__modernization_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(orth_su_t3, orthographic_kernel__modernization_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(orth_su_t6, orthographic_kernel__modernization_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__modernization_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(orth_su_t15, orthographic_kernel__modernization_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__modernization_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(orth_su_t25, orthographic_kernel__modernization_reading, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__modernization_reading, 0.25).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, ottoman_bureaucratic_expertise_devaluation).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, ulema_institutional_authority_displacement).

% DUAL FORMULATION NOTE:
% This constraint is part of the orthographic_kernel family (three readings: continuity_reading, modernization_reading, rupture_reading). Each reading instantiates a different constraint because each reading produces different ε values and beneficiary/victim structures. The modernization reading authors this constraint as tangled_rope with moderate ε (genuine technical coordination + real extraction through expertise devaluation). The continuity reading would author lower ε (script change enables access without breaking tradition). The rupture reading would author higher ε (extraction is the primary function). The three readings are not alternative observations of one constraint; they are three structurally distinct constraints arising from incompatible framings of what the orthographic kernel means. All family members must be linked via affects_constraints to enable contention analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
