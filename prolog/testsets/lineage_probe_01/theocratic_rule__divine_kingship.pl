% ============================================================================
% CONSTRAINT STORY: theocratic_rule__divine_kingship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theocratic_rule__divine_kingship, []).

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
 *   constraint_id: theocratic_rule__divine_kingship
 *   human_readable: Divine Kingship: Theocratic Rule as Cosmic Hinge
 *   domain: political/comparative_theology
 *
 * SUMMARY:
 *   Divine kingship is a theocratic form where the ruler is not merely God's
 *   interpreter or God's delegate, but the cosmic hinge itself—pharaoh as the
 *   embodied connection between cosmos and realm, the mechanism by which maat
 *   (order) flows downward and tribute/offerings flow upward. This constraint
 *   differs structurally from clerical guardianship (rule by qualified
 *   trustees interpreting divine will) and scriptural legalism (governance as
 *   application of revealed law code). In divine kingship, the ruler IS the
 *   law's source; the cosmology itself is the enforcement mechanism.
 *   Extraction operates through ontological reframing: the subject's labor is
 *   not tribute extracted by force but offering rendered to sustain cosmic
 *   order. The constraint exhibits high extractiveness (0.68) cosmologized as
 *   necessity, high suppression (0.82) embedded in an unintelligible
 *   standpoint outside the sacred mediation, and moderate theater (0.45)
 *   because the divine kingship form functions through existential claim
 *   rather than performative ritual. The theater is lower than in secondary
 *   priesthoods' ritual mediation because the ruler's cosmic presence
 *   requires no symbolic validation—it IS. Extractiveness rises over the
 *   measured interval (0.52 → 0.68) as the system matures and temple
 *   economies accumulate wealth; suppression remains high and stable as the
 *   cosmology deepens its hold.
 *
 * KEY AGENTS:
 *   - Sacred Ruler (Pharaoh, Emperor, Divine King): Primary beneficiary (institutional/arbitrage) — cosmic hinge whose being is the constraint's entire function; captures concentrated wealth flows
 *   - Temple Economy and Priestly Apparatus: Institutional beneficiary (institutional/arbitrage) — accumulates land, wealth, and tribute; derives legitimacy from the ruler's cosmic mediation
 *   - Profane Subject Populations: Primary victim (powerless/trapped) — labor and tribute reframed as cosmic offering; cannot exit without denying the ontological framework itself
 *   - Secondary Priesthood: Constrained beneficiary-victim (moderate/constrained) — depends on ruler for legitimacy but cannot challenge without dissolving its own authority
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing cosmology as law rather than constructed enforcement narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theocratic_rule__divine_kingship, 0.68).
domain_priors:suppression_score(theocratic_rule__divine_kingship, 0.82).
domain_priors:theater_ratio(theocratic_rule__divine_kingship, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theocratic_rule__divine_kingship, extractiveness, 0.68).
narrative_ontology:constraint_metric(theocratic_rule__divine_kingship, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(theocratic_rule__divine_kingship, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theocratic_rule__divine_kingship, snare).
narrative_ontology:human_readable(theocratic_rule__divine_kingship, "Divine Kingship: Theocratic Rule as Cosmic Hinge").
narrative_ontology:topic_domain(theocratic_rule__divine_kingship, "political/comparative_theology").

domain_priors:requires_active_enforcement(theocratic_rule__divine_kingship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(theocratic_rule__divine_kingship, 'da1ab3e5-3444-4b59-9e4f-4b123f8a2c34').
narrative_ontology:cs_kernel_codification('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', fixed_text).
narrative_ontology:cs_authority_grounding('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', extraction).
narrative_ontology:cs_reading_relation('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', theocratic_rule__clerical_guardianship, coexists_with).
narrative_ontology:cs_reading_relation('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', theocratic_rule__scriptural_legalism, coexists_with).
narrative_ontology:cs_axiom('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', foundational, ruler_as_cosmic_hinge).
narrative_ontology:cs_axiom_status(ruler_as_cosmic_hinge, holdable).
narrative_ontology:cs_axiom_grounding('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', ruler_as_cosmic_hinge, deontological).
narrative_ontology:cs_axiom('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', foundational, extraction_as_cosmic_offering).
narrative_ontology:cs_axiom_status(extraction_as_cosmic_offering, holdable).
narrative_ontology:cs_axiom_grounding('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', extraction_as_cosmic_offering, conventional).
narrative_ontology:cs_reference_frame('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', cosmic_mediation_framework).
narrative_ontology:cs_drift_state('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('da1ab3e5-3444-4b59-9e4f-4b123f8a2c34', '').
narrative_ontology:cs_kernel_id(theocratic_rule__divine_kingship, theocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theocratic_rule__divine_kingship, sacred_ruler).
narrative_ontology:constraint_beneficiary(theocratic_rule__divine_kingship, temple_economy).
narrative_ontology:constraint_beneficiary(theocratic_rule__divine_kingship, priestly_apparatus).
narrative_ontology:constraint_victim(theocratic_rule__divine_kingship, profane_subject_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROFANE SUBJECT (SNARE) — Trapped within a cosmology where refusal is not merely political disobedience but cosmic transgression. The subject cannot exit because exit means denying the sacred order itself. Suppression is total: any standpoint outside the sacred ruler's mediation is unintelligible, not merely forbidden. Maximum experienced extraction — the subject's own reality is reframed as cosmic necessity.
constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: SACRED RULER & TEMPLE ECONOMY (ROPE) — Experiences the constraint as pure coordination: the ruler is the hinge between cosmos and realm, the mechanism by which order flows downward and offerings flow upward. No extraction is experienced because the constraint's entire function is the ruler's own being. The temple economy benefits from concentrated wealth flows (tribute, sacrifice, land). This perspective sees the constraint as functional coordination, not coercive mechanism.
constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: SECONDARY PRIESTHOOD (SNARE) — Constrained by the sacred ruler's monopoly on cosmic access. Priests depend on the ruler for legitimacy and resources but cannot challenge the ruler without denying the cosmology that grants them authority. Extraction is asymmetric: the ruler extracts from both subject and priest. The priest's exit costs are career-ending and identity-dissolving (loss of sacred status).
constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURALIZED COSMOLOGY (MOUNTAIN) — At civilizational scale, this perspective risks treating the divine kingship cosmology as an immutable natural law: the ruler IS the cosmic hinge; the extraction IS the offering; subject and king are locked in an unchangeable ontological relationship. The engine will flag this as a false summit—the cosmology is a constructed framework that naturalizes extraction, not a law of physics. The analytical observer's native instruments cannot detect the structure that cross-position analysis reveals.
constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theocratic_rule__divine_kingship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theocratic_rule__divine_kingship, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theocratic_rule__divine_kingship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(theocratic_rule__divine_kingship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sacred ruler captures concentrated wealth flows (tribute, offerings, temple labor, land). The extraction is experienced as cosmic necessity by subjects but is structurally extraction nonetheless. The value reflects that the cosmology does not reduce the material fact of wealth asymmetry—it legitimizes it. Over the interval, extractiveness rises (0.52 → 0.68) as temple economies accumulate land and institutionalize tribute collection. This is not theater inflation (theater stays ~0.45) but real accumulation, suggesting the system is mature and consolidating. Suppression (0.82): Very high and stable. The divine kingship cosmology suppresses any standpoint outside the sacred ruler's mediation—not through explicit prohibition but through ontological closure. The subject cannot imagine exit because exit would require denying the cosmos itself. Alternative voices are not merely forbidden; they are unintelligible within the framework. Suppression is at the ceiling and remains there because the cosmology's function is exactly to prevent alternative framings. Theater (0.45): Moderate and stable. Divine kingship relies less on performative ritual than on existential claim. The ruler's cosmic presence requires no theatrical verification—it IS. Secondary priesthoods maintain higher theater because they must perform their mediating role ritually; the sacred ruler's role needs less theater because it is constitutive. The small rise (0.42 → 0.45) reflects the intensification of tributary ritual and offering ceremonies as the system matures, but theater never becomes primary because the constraint's enforcement is ontological, not performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gap between the sacred ruler (Rope) and the profane subject (Snare). The beneficiary experiences pure coordination—the constraint IS their being, their cosmic function. The subject experiences pure extraction—the constraint is the mechanism that extracts their labor and reframes it as duty. The secondary priesthood occupies an intermediate position (Snare with some Rope features)—they benefit from the structure but are themselves trapped by it, unable to challenge without losing their own authority. The analytical observer risks collapsing this gap by naturalizing the cosmology as a law of nature (Mountain), failing to see that the ontological framework is a constructed suppression mechanism. This exemplifies the oracle gap (Theorem 4): the analytical observer's native instruments cannot detect the structure that cross-position analysis reveals because the observer may internalize the cosmology's framing even while analyzing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position. The sacred ruler (beneficiary + arbitrage exit) derives d ≈ 0.05, experiencing the constraint as coordinate (-0.12 effective extraction). The profane subject (victim + trapped exit) derives d ≈ 0.95, experiencing maximum extraction (1.42 f(d)). The secondary priesthood (victim + constrained exit + partial beneficiary status) derives d ≈ 0.65, intermediate extraction. The cosmology's role is to invert these perception structures: subjects experience high suppression as natural order, not coercion; beneficiaries experience extraction as cosmic necessity, not domination. The engine's derivation chain computes d from the structural data (power, exit, beneficiary/victim); the cosmology's work is to prevent subjects from perceiving the structural reality that d quantifies.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via reading disambiguation. The constraint instantiates ONE reading of theocratic rule: divine kingship (ruler as cosmic hinge). Sibling readings (clerical guardianship, scriptural legalism) have different extractiveness profiles and suppression mechanisms. Divine kingship's extractiveness (0.68) is cosmologized—reframed as offering—which is why theater is moderate (0.45) rather than high. Clerical guardianship would show higher theater (juridical performance) and possibly lower extractiveness (rule is trusteeship, not direct extraction). Scriptural legalism would show lower suppression (law is external standard, not cosmology) and different beneficiary structure (jurists and scholars, not ruler-priests). The mandatrophy is resolved by specifying which reading: this is divine kingship, not theocracy in general. The constraint's type (Snare) flows from this specificity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmology_as_cover_vs_genuine_framework,
    'Is the divine kingship cosmology a genuine epistemic framework that participants hold, or a post-hoc rationalization of extraction dynamics?',
    'Ethnographic analysis of genuine belief vs strategic invocation. Cross-cultural comparison: do theocracies with different cosmologies show similar extraction patterns? Do subjects show signs of cognitive dissonance when the cosmology''s predictions fail?',
    'If genuine framework: suppression is internalized (cognitive lock), the constraint is identity-locked rather than trapped at the subject level, and classification shifts toward Tangled Rope. If post-hoc cover: suppression is structural (external barriers to exit), constraint remains Snare, and the cosmology is an enforcement mechanism''s narrative wrapper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmology_as_cover_vs_genuine_framework, empirical, 'Whether cosmology is genuine framework or rationalization of extraction').

omega_variable(
    priesthood_coalition_capacity,
    'Can the secondary priesthood organize to challenge the sacred ruler, or is their structure inherently atomized by the ruler''s monopoly on cosmic access?',
    'Historical case analysis: instances of priestly challenge (e.g., Aten heresy under Akhenaten, Reformation priest-scribes in late medieval Europe). Did challenges succeed? What structural preconditions enabled them? Were they directed at the ruler''s divinity or the ruler''s specific person?',
    'If priesthood can organize: potential coalition shifts the secondary priesthood from trapped to organized/constrained, enabling potential snare-to-tangled-rope transition. If atomized: priesthood remains locked in secondary extraction, snare classification from all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_coalition_capacity, empirical, 'Whether secondary priesthood can organize against sacred ruler monopoly').

omega_variable(
    cosmological_shift_mechanisms,
    'What triggers a shift from divine kingship cosmology to alternative theocratic readings (clerical guardianship, scriptural legalism)?',
    'Diachronic case study: tracking cosmological shifts in Egypt (pharaonic → Ptolemaic → Islamic), Persia (Zoroastrian kingship → Islamic theocracy), Christendom (emperor-as-vicar → papal supremacy → reformation). What material conditions (literacy spread, centralized literacy institutions, administrative complexity) correlate with cosmological transitions?',
    'Identifies whether divine kingship is a stable attractor or a transient form. If transient: extractiveness may be rising (compensatory intensification before breakdown). If stable: suppression and extractiveness remain steady. Affects trajectory projection and mandatrophy stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmological_shift_mechanisms, empirical, 'Historical triggers for cosmological shift in theocratic systems').

omega_variable(
    kernel_reading_contest_irreducibility,
    'Are divine kingship, clerical guardianship, and scriptural legalism three readings of a single contested kernel, or three distinct theocratic constraint types with incommensurable bases?',
    'Formal analysis: Can a single authority structure instantiate all three readings simultaneously? Do the three readings share a common kernel text, institutional lineage, or normative commitment? Or does commitment to one reading logically foreclose the others?',
    'If single kernel: reading_relations are coexists_with or influences (readings compete in interpretive space). If distinct types: each is a separate constraint with its own ε. This determines whether the three stored constraints are network-linked as coexistent readings or as alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_irreducibility, conceptual, 'Whether three readings instantiate one kernel or three distinct constraint types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theocratic_rule__divine_kingship, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divkng_tr_t0, theocratic_rule__divine_kingship, theater_ratio, 0, 0.42).
narrative_ontology:measurement(divkng_tr_t3, theocratic_rule__divine_kingship, theater_ratio, 3, 0.44).
narrative_ontology:measurement(divkng_tr_t6, theocratic_rule__divine_kingship, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(divkng_be_t0, theocratic_rule__divine_kingship, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(divkng_be_t3, theocratic_rule__divine_kingship, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(divkng_be_t6, theocratic_rule__divine_kingship, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divkng_su_t0, theocratic_rule__divine_kingship, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(divkng_su_t3, theocratic_rule__divine_kingship, suppression_requirement, 3, 0.8).
narrative_ontology:measurement(divkng_su_t6, theocratic_rule__divine_kingship, suppression_requirement, 6, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theocratic_rule__divine_kingship, identity_coordination).
narrative_ontology:affects_constraint(theocratic_rule__divine_kingship, theocratic_rule__clerical_guardianship).
narrative_ontology:affects_constraint(theocratic_rule__divine_kingship, theocratic_rule__scriptural_legalism).

% DUAL FORMULATION NOTE:
% Divine kingship is one of three structurally distinct readings of the contested kernel `theocratic_rule`. Clerical guardianship and scriptural legalism are alternative readings with different ε values, suppression mechanisms, and beneficiary structures. All three are coexistent in human history, held by different traditions and parties. This constraint models divine kingship; the siblings model clerical and legalist readings. Network links indicate the kernel contest, not causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
