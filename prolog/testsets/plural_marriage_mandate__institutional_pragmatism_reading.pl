% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: Plural Marriage Mandate (Institutional Pragmatism Reading): 1890 Manifesto as Strategic Doctrinal Legitimation
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto—the church leadership's public declaration suspending
 *   the practice of plural marriage—represents a critical institutional
 *   adaptation to federal coercive pressure. The institutional pragmatism
 *   reading interprets this declaration not as a legitimate prophetic
 *   reinterpretation of doctrine (the endogenous reading) or as coercive
 *   federal override of divine requirement (the exogenous reading), but as
 *   strategic institutional adaptation where doctrinal claims serve to
 *   legitimate survival-driven capitulation to superior state power. The
 *   constraint operates through a systematic gap between official doctrine
 *   and actual practice: the Manifesto announces the suspension of plural
 *   marriage as divine revelation, yet internal church records and
 *   demographic evidence document secret plural marriages continuing through
 *   the 1890s and into the early 1900s. This M-set gap (what is officially
 *   claimed vs. what actually occurs) becomes the primary observable
 *   structure. The beneficiary set includes church leadership (institutional
 *   survival, restoration of political rights, property protection) and
 *   federal authorities (achievement of policy goals through institutional
 *   capitulation while maintaining plausible deniability of coercion). The
 *   victim set includes coerced polygamists (forced to choose between
 *   doctrinal apostasy and legal destruction) and deceived monogamists
 *   (taught the Manifesto represents revelation while unaware of secret
 *   continuations). The constraint exhibits tangled rope structure: genuine
 *   institutional survival coordination entangled with doctrinal legitimation
 *   that masks coercive pressure. Theater ratio is high (0.81) because the
 *   revelation narrative itself is performative—doctrine unchanged, practice
 *   suspended, but underlying commitment sustained through secret
 *   continuations. The measurements show increasing theater and extraction
 *   from pre-1890 (extractiveness 0.32, theater 0.45) through the immediate
 *   post-Manifesto period (extractiveness 0.58, theater 0.81) and into the
 *   secret continuation era (extractiveness 0.61, theater 0.85), indicating
 *   that the constraint's extraction mechanism became entrenched rather than
 *   resolved.
 *
 * KEY AGENTS:
 *   - Church Leadership (institutional/arbitrage): Primary beneficiary — orchestrates the Manifesto as institutional survival strategy; benefits from restored political rights, property protection, and institutional legitimacy despite the gap between doctrine and practice
 *   - Federal Authorities (institutional/constrained): Secondary beneficiary — achieves policy objective (abandonment of plural marriage) while gaining plausible deniability through the church's voluntary adoption of the revelation narrative
 *   - Coerced Polygamists (powerless/trapped): Primary victims — face legal prosecution, property seizure, and forced choice between doctrinal apostasy and legal destruction; no exit available
 *   - Deceived Monogamists (powerless/trapped): Secondary victims — taught the Manifesto represents revelation; unaware of secret plural marriage continuations; suppressed from alternative framings
 *   - Analytical Observer (analytical/analytical): Sees the institutional pragmatism structure — the doctrinal cover story masking survival-driven adaptation to coercive pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.58).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate (Institutional Pragmatism Reading): 1890 Manifesto as Strategic Doctrinal Legitimation").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'c3f8d81a-99d3-45a6-b537-5b5846fda277').
narrative_ontology:cs_kernel_codification('c3f8d81a-99d3-45a6-b537-5b5846fda277', fixed_text).
narrative_ontology:cs_authority_grounding('c3f8d81a-99d3-45a6-b537-5b5846fda277', extraction).
narrative_ontology:cs_interpretation_layer_present('c3f8d81a-99d3-45a6-b537-5b5846fda277').
narrative_ontology:cs_reading_relation('c3f8d81a-99d3-45a6-b537-5b5846fda277', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3f8d81a-99d3-45a6-b537-5b5846fda277', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('c3f8d81a-99d3-45a6-b537-5b5846fda277', foundational, doctrine_practice_gap_evidences_pragmatism).
narrative_ontology:cs_axiom_status(doctrine_practice_gap_evidences_pragmatism, holdable).
narrative_ontology:cs_axiom_grounding('c3f8d81a-99d3-45a6-b537-5b5846fda277', doctrine_practice_gap_evidences_pragmatism, empirically_contingent).
narrative_ontology:cs_axiom('c3f8d81a-99d3-45a6-b537-5b5846fda277', foundational, revelation_narrative_as_legitimation_mechanism).
narrative_ontology:cs_axiom_status(revelation_narrative_as_legitimation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c3f8d81a-99d3-45a6-b537-5b5846fda277', revelation_narrative_as_legitimation_mechanism, deontological).
narrative_ontology:cs_reference_frame('c3f8d81a-99d3-45a6-b537-5b5846fda277', prophetic_authority_framework).
narrative_ontology:cs_drift_state('c3f8d81a-99d3-45a6-b537-5b5846fda277', contemporary_historical_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c3f8d81a-99d3-45a6-b537-5b5846fda277', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, federal_authorities).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED POLYGAMIST (SNARE) — Trapped between abandoning plural marriage (violating divine revelation narrative as taught pre-1890) and continuing it (federal prosecution, property seizure). Church leadership offers no genuine exit: suspension of doctrine is presented as revelation, not pragmatic capitulation. The agent bears maximum extraction — forced choice between doctrinal apostasy (from institutional teaching) or legal destruction. No coordination benefit; pure suppression of alternatives.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DECEIVED MONOGAMIST (SNARE) — Taught that the 1890 Manifesto represents legitimate prophetic reinterpretation; unaware of secret plural marriage continuations (1890-1904). Bears extraction through epistemic closure: cannot consent to or evaluate the constraint because the church leadership has withheld material information about the gap between official doctrine and actual practice. Suppression is nearly total — alternative framings (the constraint is institutional pragmatism, not revelation) are actively hidden.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL AUTHORITIES (TANGLED ROPE) — Constrained by the need to appear as neutral enforcers of law rather than religious persecutors. The church leadership's adoption of the institutional pragmatism narrative (framing the Manifesto as revelation rather than coercion) provides federal authorities with plausible deniability: they can claim the church voluntarily abandoned plural marriage through internal doctrinal development. This coordination benefit (shared narrative that obscures federal pressure) is entangled with extraction: federal authorities extract political legitimacy while the church extracts institutional survival. Both beneficiaries depend on maintaining the revelation narrative despite knowing it masks pragmatic capitulation.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHURCH LEADERSHIP (ROPE) — From the leadership's structural position, the constraint functions as coordination: the Manifesto provides a mechanism to coordinate the institution's response to federal pressure while preserving internal legitimacy through the revelation narrative. Leadership benefits through restored political rights, property protection, and institutional continuity. The constraint appears to them as a coordination solution (managing the plural marriage transition) rather than extraction — they are the designers and beneficiaries of the mechanism. The theater is high (81%) because the revelation claim itself is performative: doctrine unchanged (as internal documents show), practice suspended, but secret continuations sustained the underlying commitment.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL MEMORY / DOCTRINAL CONTINUITY (PITON) — The theological apparatus that claims the Manifesto represents legitimate prophetic reinterpretation persists through inertia and institutional investment despite the historical record showing it masked pragmatic capitulation. The constraint's function has degraded: it once served to manage the transition (immediate coordination problem). By the generational perspective, it has become performative — sustained as institutional mythology rather than functioning legitimacy mechanism. Theater ratio (0.81) reflects that the doctrinal justification now operates primarily as institutional narrative rather than as a living theological claim.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL PRAGMATISM READING (TANGLED ROPE) — From the civilizational analytical perspective, the 1890 Manifesto is a structural instance of institutional adaptation where doctrinal claims (revelation narrative) serve to legitimate survival-driven capitulation to superior coercive power. The M-set gap (doctrine unchanged, practice suspended, secret continuations 1890-1904) becomes the primary observable. This perspective sees both coordination (institutional survival mechanism) and extraction (doctrinal legitimation obscures coercive pressure, deceiving agents about the constraint's true source). The institutional pragmatism thesis identifies this as a tangled rope: genuine institutional survival coordination entangled with illegitimate doctrinal cover story.
constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plural_marriage_mandate__institutional_pragmatism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, TR),
    TR >= 0.70.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially from victims (coerced polygamists and deceived monogamists) through doctrinal cover story, forced choice, and epistemic closure. However, the extractiveness is not maximal (0.72+) because some coordination benefit genuinely exists for the institution — the church does face real federal pressure and the Manifesto does resolve an existential threat. The institutional pragmatism reading treats this as entanglement: legitimate institutional survival coordination interwoven with illegitimate doctrinal masking. Suppression (0.72): High. Suppression operates through multiple mechanisms: federal legal pressure (prosecution, property seizure, disincorporation), active concealment of the M-set gap (secret plural marriages hidden from membership), doctrinal reframing that presents coerced adaptation as revelation, and isolation from alternative interpretive communities. Coerced polygamists face suppression of all honest exit options; deceived monogamists face epistemic closure. Theater ratio (0.81): High. The revelation narrative is substantially performative. The doctrine itself is presented as unchanged (plural marriage remains doctrinally required for the highest salvific status), yet practice is publicly suspended. The secret continuations (1890-1904) document that the underlying commitment persisted despite the public narrative. The performativity increased over time as the contradiction between doctrine and practice became harder to sustain, requiring greater institutional investment in the revelation narrative's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The institutional pragmatism reading generates a sharp perspectival gap between the beneficiary (church leadership) and the victims (coerced polygamists, deceived monogamists). Church leadership perceives the constraint as coordination (Rope from their perspective) — a mechanism for managing institutional survival. Coerced polygamists perceive pure extraction and suppression (Snare) — forced choice with no honest exit. Deceived monogamists perceive institutional legitimacy based on false information (Snare via epistemic closure) — they cannot evaluate the constraint because the essential facts about the M-set gap are withheld. Federal authorities perceive institutional cooperation (Tangled Rope) — they achieve their policy goals while the church's voluntary narrative provides them political cover. The analytical observer sees the institutional pragmatism structure beneath all these perspectives: a tangled rope where genuine institutional survival coordination is entangled with doctrinal masking that illegitimately obscures the source of the adaptation (federal coercion rather than revelation). This perspectival gap is diagnostic of the institutional pragmatism reading's structural claim: the constraint works precisely because beneficiaries and victims perceive it differently, and the doctrinal narrative is the mechanism that sustains the misperception.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation reflects the beneficiary-victim structure and exit options. Church leadership, positioned as beneficiary with arbitrage exit options (they can negotiate with federal authorities or attempt institutional defiance), derives low d → low χ. Their experience of the constraint is coordination, not extraction. Federal authorities, positioned as coercing beneficiary with constrained exit (they must respond to institutional resistance or public pressure), derive moderate d → moderate χ. Their experience is achieved policy goal with political cover. Coerced polygamists, positioned as victims with trapped exit (federal prosecution if they continue, doctrinal apostasy if they comply), derive high d → high χ. Their experience is maximum extraction. Deceived monogamists, positioned as victims with trapped exit (epistemic closure prevents them from even recognizing the constraint), derive high d → high χ. Their experience is extraction through deception. The analytical observer, positioned as observer with analytical exit (ability to step outside the institutional framework), derives canonical d (0.72) → analytical χ. Their role is to recognize the structure that beneficiaries design and victims cannot escape from within the institutional narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The institutional pragmatism reading resolves mandatrophy by identifying the constraint as tangled rope: genuine institutional survival coordination (the church does face existential federal pressure) entangled with illegitimate doctrinal masking (the revelation narrative obscures coercive origin). Mandatrophy is resolved by recognizing that BOTH the coordination and extraction components are real. This is not a case where the constraint is either pure coordination (rope) or pure extraction (snare). The institutional pragmatism reading insists on holding both simultaneously: the institution genuinely needs to adapt to survive (coordination problem), AND the doctrinal cover story illegitimately obscures that coercive pressure is the origin (extraction mechanism). The conflict between the institutional pragmatism reading and its siblings (endogenous_reinterpretation_reading claims pure coordination; exogenous_override_reading claims pure extraction) is precisely where mandatrophy lives. The institutional pragmatism reading dissolves the mandatrophy by treating it as an empirical question: is the Manifesto best understood as (a) genuine revelation (endogenous), (b) coercive override (exogenous), or (c) institutional adaptation with doctrinal cover (pragmatism)? Each reading corresponds to a different classification: rope, snare, tangled_rope. The engine's classification across the kernel's three readings will reveal the structure of the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_intent_determination,
    'Did the church leadership author the 1890 Manifesto primarily as (a) genuine response to new revelation, (b) strategic institutional adaptation to federal pressure with retrospective theological framing, or (c) some mixture with weighted proportions?',
    'Analysis of pre-1890 institutional pressure (federal prosecutions, property seizures, disincorporation threats); examination of internal leadership correspondence 1885-1890; comparison of the Manifesto''s framing language with coeval institutional risk assessments; investigation of post-1890 doctrinal claims vs. documented practice (secret plural marriages 1890-1904); oral history and archival evidence regarding leadership''s subjective intent.',
    'If (a): endogenous_reinterpretation_reading is structurally accurate; this reading (institutional_pragmatism) is a mischaracterization. If (b): institutional_pragmatism_reading is accurate; the constraint is tangled_rope with doctrinal illegitimacy. If (c): the weighted mixture determines the balance between coordination (institutional survival) and extraction (doctrinal cover story); the institutional_pragmatism_reading remains structurally sound, with omegas characterizing uncertainty about magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_intent_determination, empirical, 'Was the 1890 Manifesto authored as genuine revelation response or strategic institutional adaptation?').

omega_variable(
    secret_continuation_knowledge,
    'What proportion of the church membership (polygamists, monogamists, leadership) knew about the gap between the official Manifesto (plural marriage suspended) and actual practice (secret plural marriages 1890-1904)?',
    'Analysis of historical records documenting secret marriages; testimonies from participants; archival records showing which leaders maintained/continued plural marriage relationships post-1890; demographic and genealogical reconstruction of undisclosed plural families; comparison of official vs. internal institutional narratives.',
    'If most membership was deceived: suppression gate (≥0.60) and deceived_monogamist victimhood are substantiated; epistemic closure was high. If leadership and some committed polygamists knew: suppression is lower but co-conspiracy deepens institutional extraction. The measurement shapes how transparently the constraint operated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secret_continuation_knowledge, empirical, 'Knowledge distribution regarding secret plural marriage continuations 1890-1904').

omega_variable(
    coercion_vs_persuasion_attribution,
    'Is the doctrinal framing of the Manifesto (revelation narrative) a coercive suppression of the true institutional motivation (federal pressure), or a legitimate interpretive development that happened to align with federal interests?',
    'Comparative analysis: (1) Did the church have alternative strategic responses available (e.g., sustained institutional defiance, geographic migration, legal challenge)? (2) Was the specific timing of the Manifesto (1890) driven by doctrinal discovery or federal pressure intensification? (3) Did leadership explicitly claim revelation vs. pragmatic necessity in internal vs. external communications? (4) Post-resolution institutional behavior: did the church resume plural marriage when federal pressure eased, or did the doctrinal position stabilize independently?',
    'If coercion is primary: the revelation narrative is illegitimate doctrinal cover; institutional_pragmatism_reading is correct. If persuasion/development is primary: the constraint is institutionally legitimate reinterpretation; endogenous_reinterpretation_reading is correct. The attribution determines the reading''s validity within the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_persuasion_attribution, conceptual, 'Attribution of the Manifesto''s doctrinal framing to coercive vs. legitimate institutional development').

omega_variable(
    false_summit_natural_law_risk,
    'Does the institutional_pragmatism reading naturalize what should be recognized as a historically contingent and contestable institutional choice by treating the 1890 Manifesto as a straightforward factual claim about institutional motivation?',
    'Recognition that the kernel itself (plural_marriage_mandate) is a contested commitment with three distinct readings. The institutional_pragmatism reading is ONE reading of that kernel, not a discovered fact. Its authority derives from coherent interpretation of structural evidence, not from privileged epistemic access to institutional intent. The analytical observer must remain open to the possibility that alternative readings (endogenous_reinterpretation, exogenous_override) are also legitimate interpretations of the same evidence.',
    'If this reading is treated as the uniquely correct account: false summit risk is high — institutional pragmatism itself becomes naturalized as the obvious interpretation, obscuring the kernel''s genuine contestedness. If this reading is held as one plausible interpretation among three: the kernel structure is preserved, and the analytical observer remains aware of interpretive uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether this reading risks naturalizing institutional pragmatism as the uniquely correct interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1880, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_theater_pre_announcement, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(manifesto_theater_immediate_post_1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.81).
narrative_ontology:measurement(manifesto_theater_mid_secret_period, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1897, 0.85).

% Extraction over time
narrative_ontology:measurement(manifesto_extract_pre_announcement, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(manifesto_extract_immediate_post_1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(manifesto_extract_mid_secret_period, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1897, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_suppression_pre_announcement, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(manifesto_suppression_immediate_post_1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.72).
narrative_ontology:measurement(manifesto_suppression_mid_secret_period, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1897, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel admits three structurally distinct readings, each with different ε values and classification types. The institutional_pragmatism_reading (this constraint, ε=0.58, tangled_rope) decomposes the kernel's contest into empirical and conceptual questions about institutional motivation. The endogenous_reinterpretation_reading (ε lower, rope) treats the Manifesto as legitimate prophetic development. The exogenous_override_reading (ε higher, snare) treats it as federal coercion. These are not alternative measurements of a single constraint, but three structurally distinct claims grounded in the same historical evidence. Link them via network.affects_constraints to show the kernel's structure: all three readings are live interpretations of the same contested commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
