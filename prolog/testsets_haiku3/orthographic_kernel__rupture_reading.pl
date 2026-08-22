% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Ottoman Script Rupture: Orthographic Kernel (Rupture Reading)
 *   domain: political/linguistic/commitment_system
 *
 * SUMMARY:
 *   The Turkish script reform (1928) under Atatürk is conventionally read
 *   through multiple competing narratives. This constraint story instantiates
 *   the RUPTURE READING: the script change as deliberate, coercive severance
 *   of Ottoman and Islamic cultural continuity for the purpose of
 *   constructing a new national identity decoupled from Islamic civilization.
 *   The reading frames the constraint as a high-extraction snare where the
 *   beneficiary (state apparatus + nationalist ideology) imposes massive
 *   literacy destruction on the pre-reform literate population and Islamic
 *   scholarship tradition to achieve a political objective: making the
 *   rupture visible, irreversible, and embodied in every act of literacy.
 *   This reading does NOT claim the script change was unprovoked cruelty or
 *   that coordination benefits were absent; it claims the coordination
 *   framing (scientific modernization, technical efficiency) serves as cover
 *   for extraction whose real function is identity rupture and institutional
 *   dominance. The victim set — everyone literate in the Ottoman system —
 *   experiences this as catastrophic loss of cultural capital and access. The
 *   measurement series tracks escalating suppression as the state hardens
 *   enforcement machinery, and rising extractiveness as the cost of
 *   non-compliance becomes embedded in educational and bureaucratic systems.
 *   Theater_ratio remains moderate because the state genuinely must manage
 *   new-script literacy adoption; the performative layer (public declarations
 *   of progress, pride in modernization) overlays but does not replace the
 *   functional enforcement.
 *
 * KEY AGENTS:
 *   - post_reform_state_apparatus — agenda setter, institutional power, controls educational and publishing infrastructure
 *   - ottoman_literate_population — victims, powerless, trapped by national infrastructure, lose literacy competence overnight
 *   - islamic_scholars_ulama — organized victims, identity-locked, severed from textual tradition
 *   - post_reform_generation_youth — beneficiaries, mobile, inherit cleaner national identity
 *   - ottoman_archive_preservation_community — excluded observers, would contest the rupture but suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.89).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.91).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Ottoman Script Rupture: Orthographic Kernel (Rupture Reading)").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political/linguistic/commitment_system").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, 'cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1').
narrative_ontology:cs_kernel_codification('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', fixed_text).
narrative_ontology:cs_authority_grounding('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', extraction).
narrative_ontology:cs_interpretation_layer_present('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1').
narrative_ontology:cs_reading_relation('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_axiom('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', foundational, script_change_is_deliberate_rupture).
narrative_ontology:cs_axiom_status(script_change_is_deliberate_rupture, holdable).
narrative_ontology:cs_axiom_grounding('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', script_change_is_deliberate_rupture, empirically_contingent).
narrative_ontology:cs_axiom('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', foundational, identity_severance_is_state_objective).
narrative_ontology:cs_axiom_status(identity_severance_is_state_objective, holdable).
narrative_ontology:cs_axiom_grounding('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', identity_severance_is_state_objective, deontological).
narrative_ontology:cs_reference_frame('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', ottoman_islamic_textual_continuity).
narrative_ontology:cs_drift_state('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', post_reform_period, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('cbb670e4-9ff0-4efa-a1e2-93b86d8f9ce1', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, nationalist_ideology_promoters).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholars_ulama).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, arabic_script_custodians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_generation_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the script change. Justifies the shift as modernization and Westernization but operates it as deliberate cultural rupture from Ottoman/Islamic identity. Controls education systems, official communication, publishing infrastructure. Collects political capital through demonstrable identity break and institutional control over literacy practices.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The pre-reform educated class loses literacy competence overnight. Cannot read existing Ottoman-script documents, archives, literature, religious texts, legal records. Must relearn to be functionally literate in the new script. Loses cultural capital accumulated through literacy. Exit is structurally impossible: the script is national infrastructure; remaining in the nation-state requires compliance.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_literate_population, payer,
    powerless, biographical, trapped, national).

% Guardians of Islamic textual tradition transmitted in Arabic script. The script change severes access to foundational theological texts, Quran commentary, jurisprudential literature. Their professional identity is constituted through the ability to read and transmit these texts in their original script. Relearning the new script does not restore the tradition — the rupture is the point. Some migrate; most are trapped by institutional position, age, and identity fusion with Islamic scholarship.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholars_ulama, payer,
    organized, generational, identity_locked, national).

% Calligraphers, scribes, typographers, printers whose craft is inscribed in Arabic script expertise. Their professional tools become obsolete. New employment requires retraining in Latin script typography. The rupture devalues a lifetime of accumulated skill and displaces the craft community. Some migrate to diaspora communities; most absorb the transition as occupational extinction.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, arabic_script_custodians, payer,
    moderate, generational, constrained, national).

% Educated exclusively in the new script. Gains access to European and global scientific/technical literature without translation mediation. Positioned as the vanguard of modernity and national belonging. Inherits a cleaner break from Ottoman identity, which the state frames as enabling and progressive. The benefit is real for this cohort, though constructed on the rupture imposed on their parents' generation.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_generation_youth, beneficiary,
    moderate, biographical, mobile, national).

% Intellectuals, state planners, and ideological architects who author the nation-building narrative. The script change provides visible, embodied proof of radical break from Ottoman and Islamic identity, enabling the construction of a new national self-concept decoupled from Islamic civilization. Collects political legitimacy and intellectual authority through the successful orchestration of cultural rupture.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, nationalist_ideology_promoters, beneficiary,
    institutional, generational, arbitrage, national).

% Historians, librarians, and archivists who recognize the need to preserve Ottoman-script documents and maintain literacy access to them. Systematically excluded from script-change policy design. Their voices — that rupture destroys historical continuity — are treated as reactionary and are suppressed during the reform period. Some organize to preserve documents and train script readers; most are overwhelmed by state institutional pressure.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_archive_preservation_community, excluded,
    moderate, generational, trapped, national).

% Watch the script change as evidence of Westernization and national modernization. Some provide technical assistance or ideological endorsement. None bear direct cost; the constraint is legible to them as rational nation-building. Their observational position amplifies the state apparatus's legitimacy narrative internationally.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, multinational_european_powers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The script change purportedly solves coordination around modernization and scientific/technical literacy: aligning educational infrastructure with European language systems, enabling faster adoption of Western scientific texts, positioning the nation as participant in global intellectual exchange. This framing is stated without endorsement — it is the state apparatus's own justification.
% TRANSFER_FUNCTION: Transfers cultural capital, literacy competence, archive access, and institutional position FROM the Ottoman-educated population and Islamic scholarship tradition TO the post-reform state apparatus (which controls what gets preserved, translated, or preserved-in-rupture), the nationalist ideology regime, and the post-reform-generation youth (who inherit a cleaner national identity).
% ABSENT_VOICES: Ottoman-script custodians, Islamic scholars locked into the old script, archivists and historians who would object that the rupture destroys access to foundational documents — these voices are either excluded from policy design or suppressed as reactionary obstruction. Their absence is structural, not accidental.
% DISAPPEARANCE_RATIONALE: If the script change enforcement vanished overnight — if the state ceased suppressing Arabic-script literacy and permitted dual-script access — the Ottoman literate population would recover literacy, Islamic scholarship would resume transmission in original texts, archive preservation communities would restore access, and the post-reform identity construct (built on visible rupture) would lose its embodied proof. The constraint's existence is what permits the claim that rupture is complete and irreversible.
% FOUNDING_PROBLEM: Ottoman bureaucratic infrastructure inherited from Islamic empire; Ottoman Turkish written in Arabic script tied to Islamic cultural continuity; post-WWI nation-state seeks to construct identity decoupled from Islamic civilization and synchronized with European modernity. The script change is authored as solving the problem of Ottoman identity persistence.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus attests the founding problem is live and the script change solves it. Modernization-reading proponents in the state attest the script enables scientific progress while preserving Turkish identity intact. Islamic scholars, archivists, and cultural historians — sources outside the benefiting parties — attest that the founding problem has been reframed: the real problem is not Ottoman identity persistence but the state's desire to rupture it. These external voices contest both the problem statement and the solution's necessity.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading authors very high extractiveness (0.89 at interval end) because the constraint's operation is sustained by active coercion (suppression 0.91), beneficiaries are concentrated (state + nationalist ideologues), and victims are diffuse but identifiable (literate population = entire educated class). The measurement series shows extractiveness rising steeply in the first decade (0.72→0.87) as the enforcement machinery hardens, then plateauing (0.87→0.89) as the rupture becomes normalized and the suppression cost stabilizes. Suppression rises even faster (0.73→0.91) because the state must continually prevent alternative-script literacy, suppress institutional resistance, and block archive access — the rupture is not self-maintaining; it requires continuous enforcement. Theater_ratio remains moderate (0.42) rather than high because the constraint has a genuine functional layer (schools must teach, state must communicate, youth must become literate), but that functional layer is instrumentally coupled to the extraction objective (rupture) rather than orthogonal to it. The constraint is NOT a piton (where theater dominates function); it is a snare where a real coordination problem (national literacy standardization) is weaponized to achieve extraction (cultural rupture, identity dominance). The claim/metric independence: this reading CLAIMS snare on theoretical grounds (coercive, extractive, alternatives suppressed); the authored metrics describe the actual operation (high ε, high suppression, moderate theater) independently.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus seat and the victim seats compute radically different types from this structural data. From the state's position (using the modernization_reading), the constraint solves a genuine coordination problem and enables progress — beneficiary position, low suppression narrative, Rope or Scaffold type. From the ottoman_literate_population and islamic_scholars seats (this rupture_reading), the constraint destroys literacy, imposes coercive identity erasure, and forecloses alternatives — target position, high suppression, Snare type. The engine computes these divergences from directionality: the state apparatus sits as beneficiary (d near 0.0, low/negative χ), the literate population sits as target (d near 1.0, high χ), the post-reform youth sits near symmetric (genuine coordination benefit, diffuse extraction cost). The perspectival gap emerges structurally from the asymmetric beneficiary/victim distribution, not from disagreement about facts — both readings agree the script changed, literacy was destroyed, enforcement was real. They differ on whether destruction-of-literacy is the cost of coordination or the extraction objective. This reading instantiates the latter; the modernization_reading instantiates the former.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each stakeholder: (1) post_reform_state_apparatus: institutional power, arbitrage exit, agenda-setter role → d = 0.1 (full beneficiary). (2) ottoman_literate_population: powerless, trapped exit, payer role → d = 0.95 (full target — loss of literacy and cultural capital is irreversible once enforcement embeds the script in state infrastructure). (3) islamic_scholars_ulama: organized power, identity_locked exit, payer role → d = 0.88 (near-full target; organizational capacity is offset by the identity lock that makes exit structurally unthinkable — their identity as ulama is constituted through the texts now in the old script). (4) post_reform_generation_youth: moderate power, mobile exit, beneficiary role → d = 0.35 (near-symmetric; genuine coordination benefit from new-script literacy + global science access, but diffuse cost through cultural continuity erasure — the reading frames them as benefiting from the extraction imposed on their parents). No directionality overrides are needed; the derivation chain (beneficiary/victim + power + exit) produces faithful d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman identity persistence, need for national rupture) remains contested throughout the interval. The state apparatus claims it is 'live' — Ottoman identity still threatens national coherence, continuous rupture enforcement is necessary. The archive preservation community and Islamic scholars claim it is 'dead' — the rupture was accomplished 5–10 years into enforcement; subsequent suppression is not solving the founding problem but maintaining political control. This is the classic mandatrophy pattern: the constraint persists not because the founding problem is unresolved but because the beneficiary (state) benefits from the persistence and has institutional power to block alternative readings. The high theater_ratio would suggest piton (degraded function, theatrical maintenance), but theater_ratio is moderate here because the state genuinely maintains new-script literacy as a live institutional function; the theater layer (public pride in modernization, declarations of progress) overlays rather than replaces functional enforcement. The snare classification is preserved because the founding problem's obsolescence is contested by those outside the benefiting parties, and the state's suppression machinery continues to target the old script and the scholars/archivists who would revive it — not because they threaten coordination, but because they threaten the identity construct the state built on visible rupture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_outcome,
    'Did the state apparatus INTEND the script change as deliberate cultural rupture, or is rupture an unintended consequence of genuine modernization efforts? Are the two readings describing the same event with different causal interpretations, or fundamentally different constraints?',
    'Archival evidence from state planning documents, letters, and policy debates at the moment of reform (1920s–1930s); testimony from Atatürk''s inner circle and ideological architects; comparison of stated rationales (technical/scientific) against enacted enforcement (suppression of archive access, punishment of script custodians).',
    'If intentionality is confirmed, the rupture_reading''s snare classification is robust. If rupture emerges post-hoc from modernization without explicit state intent, the reading weakens to a Rope with unintended extractive consequences — a different constraint type. If both exist (stated modernization + encoded rupture), this reading remains valid as the committer''s true causal mechanism, and the modernization_reading is the cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_outcome, empirical, 'Whether script change was intended as rupture or as unintended consequence of modernization.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.91) sustained by external enforcement machinery (police, educators, bureaucrats), or has the post-reform generation internalized the script''s dominance as natural and normal?',
    'Historical observation of post-reform society: (1) how much does suppression machinery persist 20–30 years after the script change? (2) Does the post-reform youth resist or embrace the rupture? (3) Do suppression mechanisms relax when external enforcement is reduced, or do internalized norms maintain compliance? (4) Post-exit trajectories: diaspora communities from the reform period — do they maintain Arabic-script literacy or abandon it voluntarily?',
    'If suppression is primarily external, the constraint is externally maintained; if internalized, the target population carries the suppression beyond the constraint''s reach. Internalization would indicate the rupture''s success at identity reconstruction — the younger generation no longer sees the old script as theirs but as foreign/Ottoman/Islamic. This deepens the extraction but also the trap: identity-locked victims who no longer remember they were victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in identity rupture.').

omega_variable(
    archive_loss_irreversibility,
    'Is the loss of Ottoman-script literacy and archive access PERMANENT — i.e., the rupture is irreversible after a generation — or is it structurally reversible if enforcement ceases and education redirects?',
    'Counterfactual historical analysis: if a hypothetical post-reform government (30–40 years later) chose to restore Arabic-script literacy in schools, could the pre-reform archive be read again? What institutional/technical barriers would exist? How many people would need retraining? What would restoration cost versus the cost of rupture?',
    'If the rupture is irreversible (the old-script population dies, no training mechanisms survive, archives are inaccessible), the extraction persists forever even if enforcement ceases — this is deep identity lock on a population level. If reversible, the constraint''s persistence depends on continuous suppression; if enforcement relaxes, alternatives could re-emerge. Irreversibility is the snare''s deepest trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_loss_irreversibility, empirical, 'Whether Ottoman-script literacy loss is permanent or reversible if suppression enforcement ceases.').

omega_variable(
    competing_kernel_readings_incompatibility,
    'Can the rupture_reading and the modernization_reading coexist as live readings of the same kernel, or does acceptance of the rupture reading logically foreclose the modernization reading (and vice versa)?',
    'Logical analysis: Are the core claims (modernization_reading: script change is necessary for technological progress; rupture_reading: script change is deliberately severing Islamic identity) contradictory such that no party could hold both simultaneously? Or can a single institution or party claim ''we modernized through script change AND we intentionally ruptured with Ottoman identity'' — both as true descriptions of the same event?',
    'If forecloses (one reading rules out the other), the kernel has true bifurcation and only one reading can be right. If coexists (both can be true of the same event), the kernel remains genuinely contested and three readings describe the same facts with different causal framing. The engine''s reading_relations choice (''forecloses'' vs. ''coexists_with'') depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_kernel_readings_incompatibility, conceptual, 'Logical compatibility of rupture vs. modernization readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(orth_tr_t0, projected).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(orth_tr_t5, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__rupture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(orth_tr_t20, observed).
narrative_ontology:measurement(orth_tr_t30, orthographic_kernel__rupture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(orth_tr_t30, observed).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__rupture_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(orth_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(orth_be_t0, projected).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.81).
narrative_ontology:measurement_basis(orth_be_t5, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.87).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__rupture_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(orth_be_t20, observed).
narrative_ontology:measurement(orth_be_t30, orthographic_kernel__rupture_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement_basis(orth_be_t30, observed).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__rupture_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(orth_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.73).
narrative_ontology:measurement_basis(orth_su_t0, projected).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement_basis(orth_su_t5, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__rupture_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(orth_su_t20, observed).
narrative_ontology:measurement(orth_su_t30, orthographic_kernel__rupture_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement_basis(orth_su_t30, observed).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__rupture_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement_basis(orth_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three structurally distinct constraints, each reading the same historical event (1928 script change) through different causal lenses. The rupture_reading (this file) instantiates the highest ε and the most concentrated extraction: script change as deliberate identity destruction. The modernization_reading frames the same change as genuine coordination (scientific/technical access) with unintended extractive consequences — lower ε, Rope type. The continuity_reading (from the post-reform perspective) frames the change as violation of natural cultural law — Mountain type. Each reading has its own victim/beneficiary set, directionality, and type classification. They are linked as a kernel family because the three readings all interpret the same historical kernel (the state's decision to change scripts), and acceptance of one reading produces structural pressure on the others (influences relation, not forecloses — the readings coexist across different parties rather than foreclosing each other logically).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
