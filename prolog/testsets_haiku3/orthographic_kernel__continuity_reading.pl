% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script as Ottoman Cultural Continuity Anchoring Mechanism
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   In the Ottoman empire (roughly 14th–early 20th century), the exclusive
 *   use of Arabic script for all formal administration, Islamic
 *   jurisprudence, scholarly communication, and textual authority preserved
 *   institutional continuity with Islamic civilization and safeguarded the
 *   monopoly of the literate elite on interpreting law and doctrine. From the
 *   continuity reading's frame, this constraint solves the genuine
 *   coordination problem of maintaining institutional memory and textual
 *   authority across generations in a multilingual, multi-ethnic empire. From
 *   alternative readings (modernization, rupture), the same constraint is
 *   extractive gatekeeping that locked mass populations out of literacy and
 *   technical knowledge. This story instantiates the continuity reading only,
 *   describing the constraint as it appears from that frame's own lights: as
 *   preservation of something essential, at the cost of slower technical
 *   modernization and restricted access to bureaucratic mobility for
 *   non-elites.
 *
 * KEY AGENTS:
 *   - Ottoman literate elite (scribes, judges, administrators): beneficiary + identity-locked + institutional power
 *   - Islamic institutional authority (Quranic scholars, legal jurists, religious institutional bodies): beneficiary + identity-locked + civilizational time horizon
 *   - Ottoman lower classes and provincial literacy seekers: victims + trapped/constrained exit + powerless to moderate power
 *   - State modernizers and technical educators (excluded voices): would challenge the constraint; structurally outside the continuity frame
 *   - Turkish national identity architects (observers): later overturn the constraint; at this interval's peak, they are analytical observers of a constraint they will dissolve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.72).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script as Ottoman Cultural Continuity Anchoring Mechanism").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '274dbc52-2b3e-4d60-9b92-34eda9fe35c7').
narrative_ontology:cs_kernel_codification('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', formalized).
narrative_ontology:cs_authority_grounding('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', lineage).
narrative_ontology:cs_interpretation_layer_present('274dbc52-2b3e-4d60-9b92-34eda9fe35c7').
narrative_ontology:cs_reading_relation('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', foundational, islamic_continuity_necessary_preservation).
narrative_ontology:cs_axiom_status(islamic_continuity_necessary_preservation, holdable).
narrative_ontology:cs_axiom_grounding('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', islamic_continuity_necessary_preservation, deontological).
narrative_ontology:cs_axiom('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', secondary, script_singularity_ensures_textual_integrity).
narrative_ontology:cs_axiom_status(script_singularity_ensures_textual_integrity, holdable).
narrative_ontology:cs_axiom_grounding('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', script_singularity_ensures_textual_integrity, empirically_contingent).
narrative_ontology:cs_reference_frame('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', ottoman_islamic_institutional_lineage).
narrative_ontology:cs_drift_state('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', late_ottoman_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('274dbc52-2b3e-4d60-9b92-34eda9fe35c7', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_elite).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, islamic_institutional_authority).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_lower_classes).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, provincial_literacy_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, quranic_interpreter_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Ottoman court, administrative bureaucracy, and religious scholars whose literacy, social standing, and institutional authority depend entirely on mastery of Arabic script and the cultural/linguistic knowledge it encodes. Script continuity preserves their monopoly on textual interpretation, administrative authority, and cultural legitimacy. Their identity as learned persons is constitutively fused with the script; abandoning it would dissolve their claim to custodianship of Ottoman tradition.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_elite, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ottoman_literate_elite, agenda_setter).

% Islamic legal scholars, Quranic interpreters, and religious institutions whose authority rests on direct textual access to Arabic scripture and Islamic jurisprudence. Script preservation ensures the Quran and Islamic texts remain accessible only through their authorized interpretation. A script change could democratize Quranic literacy, weakening their gatekeeping authority and legitimacy claim as sole custodians of Islamic knowledge.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_institutional_authority, beneficiary,
    organized, civilizational, identity_locked, universal).

% Urban artisans, rural peasants, merchant-class figures without scribal training, and women excluded from formal education. Kept functionally illiterate by the script barrier: learning Arabic requires years of formal study unavailable to non-elite populations. Their exclusion from literacy and text-based power is maintained by the script's structural difficulty and the institutional monopoly on its teaching.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_lower_classes, payer,
    powerless, biographical, trapped, continental).

% Provincial merchants, minor officials, and educated non-elites in provincial towns who aspire to administrative or mercantile positions requiring literacy. Face prohibitive costs to acquire Arabic script fluency. Their mobility and social advancement is structurally constrained by gatekeeping on script mastery; they bear the cost of extended education while remaining subordinate to the scribal elite.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, provincial_literacy_seekers, payer,
    moderate, biographical, constrained, regional).

% Late Ottoman reformers, Young Turk modernizers, and military-technical modernizers who view the literacy barrier as an impediment to rapid technical education, military training, and industrial development. Would argue for Latin script adoption to enable faster mass literacy, faster technical knowledge transfer, and integration with European scientific and technical literature. Their position is structurally excluded from the script-continuity framework: to advocate script change is to accept the rupture reading rather than the continuity reading.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernization_coalition, excluded,
    powerful, biographical, constrained, continental).

% Professional Quranic scholars and hadith specialists whose interpretive authority depends on the assumption that true Islamic knowledge requires mastery of classical Arabic as written in traditional script. Script preservation ensures their interpretive monopoly persists; script change would allow lay readers direct access to Quranic text without mediation through the scholarly class.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, quranic_interpreter_class, beneficiary,
    organized, civilizational, identity_locked, universal).

% European educational and technical institutions, scientific societies, and military academies that use Latin script for dissemination. From their seat, the Arabic script requirement for Ottoman students seeking technical training represents a friction cost. They do not enforce the constraint but document its effects: Ottoman students require additional preparation time to acquire script fluency before accessing technical literature.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, western_technical_standard_setters, observer,
    institutional, biographical, analytical, global).

% Republican-era intellectuals and state officials tasked with constructing a post-Ottoman Turkish national identity. Their analytical position observes the continuity-reading constraint as an obstacle to modernization without being directly subject to it. Their later policy choice (script reform in 1928) represents the victory of an alternative reading over the continuity frame, but at the time of this constraint's peak operation, they are excluded observers of a constraint they will eventually overturn.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, turkish_national_identity_architects, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, ottoman_literate_elite).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional access to Ottoman administrative tradition, Islamic textual authority, and cultural-linguistic continuity with the preceding centuries of Islamic civilization. A unified script enables bureaucratic continuity, allows reuse of administrative precedent and legal doctrine, and anchors Ottoman legitimacy in Islamic institutional tradition. The constraint solves the coordination problem of maintaining institutional memory and textual authority across generational transitions within a multi-ethnic empire.
% TRANSFER_FUNCTION: Transfers literacy capital, interpretive authority, and administrative power from the broader population to the scribal-judicial elite. Moves the cost of acquiring literacy from the state (if mass education in a simpler script were provided) to individual learners (who must spend years acquiring Arabic fluency). Moves interpretive authority over Islamic law and Quranic meaning exclusively to the trained elite class, preventing lay access to textual sources.
% ABSENT_VOICES: State modernizers, technical educators, and mass-literacy advocates are structurally excluded from the continuity reading's framework. So are the Ottoman lower classes and provincial literacy seekers themselves — they have no voice in the institutional conversation about script because the script barrier prevents their participation in the institutions (courts, administrative bodies, scholarly circles) where such decisions are made. Their exclusion is not incidental; it is constitutive of how the constraint operates.
% DISAPPEARANCE_RATIONALE: If Arabic script continuity were abandoned overnight — replaced by a phonetically simpler script — the Ottoman bureaucracy would face an immediate archival crisis (existing legal precedent, administrative records, and land registries would become inaccessible to new administrators without training). Institutional authority would have to be reconstructed. The Islamic scholarly class would lose its gatekeeping power over Quranic interpretation. Mass literacy would become attainable in years rather than decades, shifting the social distribution of textual authority. The empire's claim to continuity with Islamic civilization would become contestable. An enormous reorganization would follow.
% FOUNDING_PROBLEM: How do we preserve institutional continuity with the Islamic scholarly tradition, safeguard the integrity of Quranic texts, and maintain administrative access to centuries of Ottoman legal and administrative precedent in a multilingual, multi-ethnic empire spanning three continents?
% FOUNDING_PROBLEM_CORROBORATION: Ottoman institutional scholars and Islamic legal authorities attest the problem is live and the script provides the solution. Late-19th and early-20th century state reformers attest the problem has been overshadowed by a NEW problem — mass technical literacy is now the binding constraint on military and industrial modernization, making the founding problem's solution (script continuity) increasingly costly. Republican-era intellectuals and the Turkish state after 1928 attest the founding problem became obsolete once Ottoman empire dissolved and Turkish national identity was decoupled from Islamic institutional authority. No neutral external corroboration exists; the founding problem's status depends entirely on whether one accepts the continuity frame (problem live) or the modernization frame (problem solved, new priority emerged).
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the script barrier prevents mass literacy and keeps administrative authority concentrated in a trained elite, but the barrier is defended as preservation of continuity rather than pure gatekeeping — hence it is not at snare level (0.85+). Suppression is also high (0.72) because the constraint's persistence depends on active institutional enforcement: the elite must prevent script simplification, must control access to training, must defend the script against calls for reform. The suppression requirement increases from t0 to t15 as modernization pressure builds, then plateaus — at some point suppression becomes costly enough that the constraint approaches instability (which historically occurs at the 1928 Turkish script reform). Theater ratio rises from 0.25 to 0.41 as the constraint's functional justification (administrative continuity) becomes less credible and more energy goes into defending the script as a cultural symbol. The measurement series uses one aligned time grid: every metric is authored at every time point (0, 5, 10, 15, 20, 25) so the engine can detect type transitions and theater drift without imputation.
 *
 * PERSPECTIVAL GAP:
 *   The Ottoman literate elite and the agenda_setter seat (beneficiary + custodian) would perceive this constraint as genuine coordination: it preserves something real, solves a real problem, and is justified. From the victim seats (lower classes, provincial literacy seekers), the same constraint is coercive exclusion — they are locked out of literacy by institutional gatekeeping, not by natural difficulty. The engine should compute the elite's seat as Rope or low-extraction Tangled Rope (coordination with incidental capture), while the lower-class seat computes as high-extraction Snare (pure exclusion). The perspectival divergence is the point: the same structural arrangement produces different experienced types depending on power, exit options, and benefit flow. The authored claim (Tangled Rope) reflects the constraint's structural makeup — it has BOTH coordination function (preservation) AND asymmetric extraction (gatekeeping). Neither seat alone captures the full picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Ottoman literate elite: role=beneficiary+agenda_setter, power=institutional, exit_options=identity_locked. This gives directionality near 0.0 (full beneficiary). They set the rules, they benefit from the monopoly, and they cannot exit without dissolving their identity as custodians of tradition. Islamic institutional authority: role=beneficiary, power=organized, exit_options=identity_locked, time_horizon=civilizational. Directionality near 0.0 as well — they collect the gatekeeping power and their entire interpretive legitimacy fuses with Arabic script mastery. Ottoman lower classes: role=payer, power=powerless, exit_options=trapped, time_horizon=biographical. Directionality near 1.0 (full target) — they bear the cost (exclusion from literacy), have no power to change the rule, and cannot exit (trapped in their social position). Provincial literacy seekers: role=payer, power=moderate, exit_options=constrained. Directionality around 0.75 — they pay the cost (extended education, constrained mobility), have some institutional power (minor officials can advocate for script reform) but cannot successfully exit the constraint without joining the elite class they need the script to access. The asymmetry is structural: beneficiaries and victims sit at opposite ends of the directionality spectrum.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as Tangled Rope because it genuinely coordinates institutional memory and textual authority while extracting gatekeeping power. As we move rightward in the measurement series (t0 to t25), theater_ratio rises from 0.25 to 0.41, indicating the functional justification (coordination) is weakening relative to the performative justification (cultural preservation). At what point does the constraint tip from Tangled Rope (coordination + extraction) to Piton (inertial performance)? The theater threshold is around 0.5, so by t25 the constraint is still clearly Tangled Rope on the metrics. However, the historical record shows that by 1928 (post-interval), the constraint was abandoned entirely — suggesting that the theater rise would have continued and crossed the Piton threshold shortly after t25. The mandatrophy is building but not yet resolved in this interval: the founding problem (institutional continuity) is still held as live by the benefiting parties, but the excluded modernizers have made it clear that a NEW problem (technical modernization) has superseded the founding problem. This shift in problem-priority is what eventually kills the constraint, not immediate theater collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression of lower-class literacy primarily structural (state/elite institutions provide no access to Arabic script training for non-elites) or internalized (lower classes accept the script barrier as natural/inevitable and do not seek literacy)?',
    'Historical comparison with regions that offered simplified-script literacy pathways (e.g., Persian in Iran): if lower-class demand for literacy emerged rapidly once barriers were lowered, suppression was primarily structural; if demand remained low even with barriers removed, internalization was significant.',
    'If suppression is mostly structural, the constraint operates as coercive exclusion and the effective extraction is as authored. If internalized, the target population has absorbed the constraint so thoroughly that removal of structural barriers would not immediately restore them to the beneficiary class — the internalized suppression would persist post-exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression of lower-class literacy is structural gatekeeping or internalized acceptance.').

omega_variable(
    continuity_vs_constructed_tradition,
    'Is the claimed Ottoman-Islamic continuity a genuine institutional lineage (Ottoman bureaucracy materially descended from Islamic administrative tradition, Quranic scholarship genuinely inaccessible without Arabic), or is the continuity partly constructed post-hoc to justify elite gatekeeping (the claim that script preserves something that could not be preserved through other means)?',
    'Textual analysis of actual administrative reuse and knowledge transfer: does Ottoman bureaucracy materially depend on access to pre-Ottoman Islamic legal precedent, or is the precedent invoked rhetorically? Could Islamic knowledge be preserved and transmitted through translation/simplified script without loss?',
    'If continuity is genuine material necessity, part of the measured extraction is the price of coordination and preservation. If continuity is constructed narrative, the extraction is mostly pure gatekeeping without coordination function — a snare mischaracterized as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_constructed_tradition, conceptual, 'Whether script continuity is functionally necessary for institutional memory or partly a justification for elite control.').

omega_variable(
    reading_decomposition_modernization_vs_continuity,
    'Is this constraint one constraint viewed from two angles (continuity as the official reading, modernization as the challenger reading of the same rule)? Or are these two structurally distinct constraints — one preserving Ottoman elite authority (ε high), one enabling technical modernization (ε different)?',
    'The schema''s constraint decomposition rule (ε-invariance): measure the constraint under the continuity reading (standing Ottoman arrangement) and under the modernization reading (Turkish technical/scientific advancement pathway). If ε differs substantially between readings, they are different constraints and should be authored separately per the decomposition rule.',
    'This constraint is authored ONLY under the continuity reading (ε=0.68, standing Ottoman arrangement under contest from the continuity frame''s own lights). The modernization reading would author a different ε over the same referent (standing arrangement) or decompose into two files per the ε-invariance principle. Treating them as readings of one kernel is appropriate; treating them as two ε values for one constraint violates the invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_decomposition_modernization_vs_continuity, conceptual, 'Whether this is one constraint with multiple readings or two constraints requiring decomposition.').

omega_variable(
    identity_lock_mechanism_ottoman_elite,
    'What specific mechanism binds Ottoman literate elite to Arabic script identity? Is it professional identity (scribal career paths require Arabic mastery)? Relational identity (the elite''s self-concept and status depends on being the custodians of a tradition)? Ideological identity (a worldview in which Islamic authenticity = Arabic script access)? Institutional identity (the elite has become constitutively identified with the institution of script-mediated authority)?',
    'Historical analysis of whether elite individuals who learned Latin script (some did, for technical/military reasons) retained their social status and institutional authority, or whether script mastery was the hinge of their legitimacy.',
    'If professional identity alone, elite could theoretically switch to Latin script while retaining their careers (hard but possible). If relational/ideological/institutional identity, script exit would dissolve their claim to be the authentic custodians of Ottoman tradition and Islamic knowledge — the identity exit would be as costly as the professional exit. The type of identity lock affects the directionality: professional-only locks yield lower d; relational/ideological/institutional locks yield higher d (closer to full target).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ottoman_elite, empirical, 'Which identity-fusion mechanism binds the Ottoman literate elite to Arabic script.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__continuity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__continuity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(orth_tr_t15, orthographic_kernel__continuity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(orth_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(orth_tr_t25, orthographic_kernel__continuity_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__continuity_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__continuity_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(orth_be_t15, orthographic_kernel__continuity_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(orth_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(orth_be_t25, orthographic_kernel__continuity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__continuity_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__continuity_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(orth_su_t15, orthographic_kernel__continuity_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(orth_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(orth_su_t25, orthographic_kernel__continuity_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_kernel contested commitment. The same standing arrangement (Ottoman use of Arabic script) is authored under three different readings, each with its own ε and political interpretation. Continuity_reading emphasizes preservation and institutional memory (ε high for elite, coordination function real but gatekeeping costs concentrated). Modernization_reading would emphasize technical access and rapid literacy (ε high for technical modernizers, low for script-change resisters). Rupture_reading would emphasize intentional identity break (ε measuring cultural severing, beneficiary set different). Each reading instantiates a different constraint; they are related by being readings of one kernel rather than by sharing ε or victim/beneficiary structure. See commentary.kernel_context for the reading frame and cs_structure for the reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
