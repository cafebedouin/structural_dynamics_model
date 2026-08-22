% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Textualist Reading of Usul al-Fiqh Method (Maximal Textual Restrictiveness, Minimized Qiyas, Sadd al-Dhara'i)
 *   domain: religious/legal/jurisprudential
 *
 * SUMMARY:
 *   This story is one reading of a contested kernel — the usul al-fiqh method
 *   for deriving Islamic law from its sources. The Hanbali reading maximizes
 *   reliance on Quran and authenticated hadith, minimizes qiyas to cases of
 *   genuine textual silence, prefers even weak hadith over reasoned analogy,
 *   and deploys sadd al-dhara'i (blocking the means to potential innovation)
 *   to preserve textual fidelity against drift. This is generated as a
 *   single, ε-invariant constraint under Rule 1: it does not describe or
 *   average over the sibling readings (Hanafi's expansive qiyas/istihsan,
 *   Maliki's integration of Medinan practice and maslaha mursala, Shafi'i's
 *   systematized hierarchy privileging authenticated hadith over qiyas). Each
 *   sibling is a separate constraint with its own ε, linked here via
 *   network.affects_constraints and cs_structure.reading_relations, not
 *   folded into this reading's classification.
 *
 * KEY AGENTS:
 *   - hanbali_textualist_scholars: agenda_setter/beneficiary (institutional/identity_locked) — administer the method, authenticate hadith, invoke sadd al-dhara'i
 *   - traditionist_hadith_transmitters: beneficiary (organized/identity_locked) — their transmitted material gains outsized evidentiary weight
 *   - anti_innovation_reform_movements: beneficiary (organized/constrained) — use the method to delegitimize rival practices as bid'a
 *   - rationalist_jurists: payer (moderate/constrained) — analogical and preference-based reasoning sharply narrowed
 *   - customary_law_communities: payer (powerless/trapped) — local practice denied independent evidentiary standing
 *   - jurists_facing_novel_cases: payer (moderate/constrained) — fewer tools for principled extension to new situations
 *   - regional_legal_pluralists: excluded (powerless/trapped) — structurally unheard within the source hierarchy
 *   - comparative_legal_historians: observer (analytical/analytical) — trace the reading's downstream substantive effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.58).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.62).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textualist Reading of Usul al-Fiqh Method (Maximal Textual Restrictiveness, Minimized Qiyas, Sadd al-Dhara'i)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal/jurisprudential").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '1807fc6e-4bc1-40a3-8b2c-52d6369af799').
narrative_ontology:cs_kernel_codification('1807fc6e-4bc1-40a3-8b2c-52d6369af799', fixed_text).
narrative_ontology:cs_authority_grounding('1807fc6e-4bc1-40a3-8b2c-52d6369af799', lineage).
narrative_ontology:cs_interpretation_layer_present('1807fc6e-4bc1-40a3-8b2c-52d6369af799').
narrative_ontology:cs_reading_relation('1807fc6e-4bc1-40a3-8b2c-52d6369af799', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1807fc6e-4bc1-40a3-8b2c-52d6369af799', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1807fc6e-4bc1-40a3-8b2c-52d6369af799', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('1807fc6e-4bc1-40a3-8b2c-52d6369af799', foundational, textual_silence_is_narrow_and_rare).
narrative_ontology:cs_axiom_status(textual_silence_is_narrow_and_rare, holdable).
narrative_ontology:cs_axiom_grounding('1807fc6e-4bc1-40a3-8b2c-52d6369af799', textual_silence_is_narrow_and_rare, conventional).
narrative_ontology:cs_axiom('1807fc6e-4bc1-40a3-8b2c-52d6369af799', foundational, weak_hadith_outranks_reasoned_analogy).
narrative_ontology:cs_axiom_status(weak_hadith_outranks_reasoned_analogy, holdable).
narrative_ontology:cs_axiom_grounding('1807fc6e-4bc1-40a3-8b2c-52d6369af799', weak_hadith_outranks_reasoned_analogy, conventional).
narrative_ontology:cs_axiom('1807fc6e-4bc1-40a3-8b2c-52d6369af799', secondary, blocking_means_to_innovation_is_obligatory).
narrative_ontology:cs_axiom_status(blocking_means_to_innovation_is_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('1807fc6e-4bc1-40a3-8b2c-52d6369af799', blocking_means_to_innovation_is_obligatory, instrumental).
narrative_ontology:cs_reference_frame('1807fc6e-4bc1-40a3-8b2c-52d6369af799', early_traditionist_textual_primacy).
narrative_ontology:cs_drift_state('1807fc6e-4bc1-40a3-8b2c-52d6369af799', contemporary_salafi_revivalism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1807fc6e-4bc1-40a3-8b2c-52d6369af799', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, traditionist_hadith_transmitters).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, anti_innovation_reform_movements).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, jurists_facing_novel_cases).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, regional_legal_pluralists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the interpretive method: they determine which hadith count as authenticated, when textual silence genuinely obtains before qiyas may be invoked, and which juristic innovations count as bid'a to be blocked via sadd al-dhara'i. Their scholarly authority and communal standing are constituted by fidelity to this textualist method; abandoning it would dissolve their distinguishing claim within the broader legal tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, beneficiary).

% Their labor — collecting, grading, and transmitting hadith, including weak hadith preferred here over qiyas — is elevated to primary evidentiary status by this method. The reading validates their vocation and gives their transmitted material more legal weight than reasoned analogy would in other readings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, traditionist_hadith_transmitters, beneficiary,
    organized, generational, identity_locked, regional).

% Draw on sadd al-dhara'i and textual maximalism to argue against practices they characterize as later accretions (bid'a). The method gives them a ready-made doctrinal lever to delegitimize competing customary or rationalist practices as departures from revealed sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, anti_innovation_reform_movements, beneficiary,
    organized, generational, constrained, regional).

% Jurists inclined toward expansive analogical reasoning (as in the Hanafi reading) or independent juristic preference find their preferred tools sharply narrowed under this method — qiyas is permitted only at the margins of clear textual silence, and their reasoned conclusions can be displaced by a weak hadith. Their intellectual production is devalued relative to raw textual transmission; exiting means working within a different school's community, at real cost to standing and audience.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Local populations whose inherited customary practices ('urf) carry no independent evidentiary weight under this reading (contrast with the Maliki reading's integration of 'amal and 'urf). Where custom is judged to conflict with — or merely to lack clear grounding in — text, sadd al-dhara'i can be invoked to block it, regardless of its long-standing social function. They cannot easily relocate their community's practice into a different legal jurisdiction.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_communities, payer,
    powerless, generational, trapped, local).

% Confront genuinely novel situations (new financial instruments, technologies, social arrangements) where the method's minimization of qiyas and preference for weak hadith over reasoned analogy leaves fewer tools for principled extension, sometimes producing outcomes felt as arbitrary or as blocking beneficial innovation because a directly permissive precedent cannot be textually located.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, jurists_facing_novel_cases, payer,
    moderate, biographical, constrained, regional).

% Advocates of accommodating regional legal diversity (analogous to Maliki 'amal ahl al-Madina reasoning) have no standing within this method's source hierarchy to argue that place-based practice should carry evidentiary weight; their objection is structurally unheard because the method's premises exclude the category of evidence they would invoke.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, regional_legal_pluralists, excluded,
    powerless, generational, trapped, regional).

% Study the four readings comparatively, tracing how each reading's source hierarchy produces different substantive law and different distributions of interpretive authority, without themselves being bound by any single school's method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, textually anchored decision procedure for deriving law that resists drift toward unconstrained juristic discretion — reducing the risk that legal outcomes track a jurist's individual preference rather than revealed sources, and giving the community a predictable, textually traceable basis for legal certainty.
% TRANSFER_FUNCTION: Moves interpretive authority and evidentiary weight away from reasoned analogy, customary practice, and juristic preference, and toward hadith transmission and textualist scholarship — shifting who counts as a legitimate source of law from communities and rationalist jurists toward traditionist scholars and hadith critics.
% ABSENT_VOICES: Customary law communities and legal pluralists who would argue that long-standing local practice or regional consensus deserves independent evidentiary standing are not represented in the method's source hierarchy at all — the hierarchy itself defines their evidence out of consideration, so they cannot object from within the framework, only from outside it.
% DISAPPEARANCE_RATIONALE: If this specific textualist reading disappeared, Hanbali-affiliated legal communities would either converge toward one of the sibling readings (more qiyas, more custom, more consensus-based reasoning) or fracture into competing local methods; the doctrinal basis for blocking specific customary and rationalist practices via sadd al-dhara'i would lose its grounding, and practices currently excluded as bid'a would re-enter live legal contest.
% FOUNDING_PROBLEM: Early juristic disputes in the 8th-9th centuries CE saw expansive use of ra'y (personal reasoned opinion) and regional custom producing what traditionists viewed as legal outcomes untethered from, or in tension with, revealed text — Ahmad ibn Hanbal and successors sought to anchor law maximally to Quran and authenticated hadith to guard against both individual caprice and gradual doctrinal drift.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars themselves attest the problem is permanently live — unrestrained reasoning is treated as a perennial risk requiring constant textual discipline, not a historically bounded dispute. Comparative legal historians and jurists from sibling schools (particularly Hanafi and Maliki commentators) attest that the founding dispute over ra'y's proper scope was substantially resolved through the broader development of systematized usul al-fiqh across all schools by the classical period, and that continued maximal restrictiveness now functions as much to preserve a distinct communal and scholarly identity as to solve an active interpretive crisis.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.58 — the method genuinely solves a real coordination problem (anchoring legal derivation to a stable, textually traceable source hierarchy, resisting unconstrained juristic discretion) but does so by systematically transferring interpretive authority away from customary and rationalist actors toward scholars administering hadith authentication and sadd al-dhara'i determinations, who face no comparable check on their own judgment calls about what counts as 'clear textual silence' or 'blocking the means.' Suppression (0.62) reflects that this is not merely a preference but an actively enforced doctrinal boundary — deviations are labeled bid'a, a strong communal and theological sanction. Theater ratio is comparatively low (0.28): the textualist apparatus (hadith science, chains of transmission, sadd al-dhara'i reasoning) is substantively operative, not merely performed, though it has grown somewhat more elaborated and codified over the interval. Accessibility collapse (0.6) is high because the method's own premises exclude customary and analogical evidence from counting at all, not merely disfavoring them — once inside the tradition, those alternatives are hard to argue for on the tradition's own terms.
 *
 * PERSPECTIVAL GAP:
 *   From the hanbali_textualist_scholars' seat, this reading is a rope-like coordination achievement: a defense against interpretive drift and personal caprice, serving the whole community's interest in legal certainty and revealed authority. From the seats of rationalist_jurists, customary_law_communities, and regional_legal_pluralists, the same method operates as a structural closure — their evidentiary resources are defined out of the hierarchy, and 'blocking innovation' functions to foreclose live legal development they would otherwise pursue. The engine should compute these seats differently from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   hanbali_textualist_scholars, traditionist_hadith_transmitters, and anti_innovation_reform_movements are declared beneficiaries: the method elevates their evidentiary category (hadith, textual literalism) and their authority to police bid'a, pulling their directionality toward the beneficiary end. rationalist_jurists, jurists_facing_novel_cases, and customary_law_communities are declared victims: their preferred tools (qiyas, reasoned preference, custom) are narrowed or excluded, pulling directionality toward the target end — customary_law_communities most sharply, given trapped exit and powerless standing relative to moderate-power jurists who at least retain some intra-tradition maneuvering room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration fields register a genuine mismatch: the scholars who administer the method attest the founding problem (unrestrained ra'y) remains permanently live, while corroboration from sibling-school jurists and comparative historians suggests the core dispute was substantially resolved through the broader classical development of usul al-fiqh across all schools, and that continued maximal restrictiveness now serves communal-identity maintenance as much as active problem-solving. This mismatch (status=contested rather than clean live/dead, verdict=world_rearranges) is exactly the signal the R5 genealogy interview is built to surface — it does not resolve the mandatrophy question but documents it as open rather than asserting the scholars' self-account as settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualist_reading_kernel_indeterminacy,
    'Is the Hanbali reading''s maximal textual restrictiveness a faithful recovery of the earliest, least-corrupted juristic method, or a historically contingent methodological choice among several defensible ways of weighting sources that later hardened into school identity?',
    'Comparative historical analysis of pre-school juristic practice (2nd-3rd century AH) against the four schools'' later systematized methods, examining whether the classical sources themselves underdetermine the choice among qiyas-expansive, custom-integrating, and text-maximal approaches.',
    'If the kernel is genuinely underdetermined at the founding, the Hanbali reading''s claim to superior textual fidelity is a contestable interpretive stance rather than a discovered fact, strengthening the case that this reading functions partly as an in-group identity marker (supporting the tangled_rope classification) rather than pure recovered orthodoxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_reading_kernel_indeterminacy, conceptual, 'Whether textual maximalism recovers an original method or represents one contingent reading among several defensible ones.').

omega_variable(
    sadd_al_dharai_scope_ambiguity,
    'Is the scope of sadd al-dhara''i (blocking means to potential harm/innovation) determined by clear, bounded textual criteria, or does its application depend substantially on the discretionary judgment of the scholars administering it — making it a vector for exactly the kind of unconstrained juristic discretion the method claims to guard against?',
    'Analysis of historical fatwa and legal rulings invoking sadd al-dhara''i, comparing scholarly consensus versus disputed applications across similar fact patterns to assess how much discretion the doctrine actually leaves to its administrators.',
    'If application is substantially discretionary, sadd al-dhara''i functions as a relocated site of juristic power rather than a genuine textual constraint, supporting higher effective extraction than the surface doctrine suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dharai_scope_ambiguity, empirical, 'Whether sadd al-dhara''i is textually bounded or a vector for discretionary scholarly power.').

omega_variable(
    weak_hadith_preference_justification,
    'Does preferring weak hadith over qiyas genuinely track a principled preference for textual (even if imperfectly authenticated) sources over human reasoning, or does it function to preserve the evidentiary primacy and social standing of the traditionist/hadith-transmission community against rationalist competitors?',
    'Examine cases where weak-hadith rulings produced outcomes divergent from what careful qiyas would have yielded, and assess whether the stated rationale (textual primacy) or the effect (traditionist authority preservation) better predicts which doctrine wins in contested cases.',
    'If effect better predicts outcome than stated rationale, the weak-hadith-preference component is better modeled as extraction favoring a specific scholarly constituency than as neutral methodological principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weak_hadith_preference_justification, empirical, 'Whether weak-hadith preference is principled textualism or traditionist guild preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanbali_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanbali_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__hanbali_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__hanbali_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the usul_al_fiqh_method kernel (hanafi_reading, hanbali_reading, maliki_reading, shafii_reading). Each reading has its own ε, beneficiary/victim structure, and classification, reflecting genuinely different methodological commitments about source hierarchy rather than different measurements of one constraint. The Hanbali reading has the highest textual restrictiveness and lowest analogical/customary scope among the four; the Hanafi reading sits at the opposite pole with expansive qiyas and istihsan. Network edges here register structural influence (each school's method shapes the intellectual and political environment the others operate in, and historically these schools contested legal authority in overlapping jurisdictions) rather than shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
