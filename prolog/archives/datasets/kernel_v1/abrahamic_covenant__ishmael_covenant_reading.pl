% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Abrahamic Covenant through Ishmael: Inclusive Reading of Genesis Promise
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   The Ishmael-inclusive reading of the Abrahamic covenant claims that
 *   Genesis 17 establishes covenant with Ishmael as well as Isaac, and that
 *   this covenant continues through Ishmael's descendants to Muhammad and the
 *   Islamic tradition. This reading challenges the classical Jewish
 *   interpretation of covenant as exclusive to Isaac and his line. It creates
 *   a tangled_rope structure: it genuinely solves coordination problems
 *   (enabling interfaith dialogue, collapsing zero-sum theological
 *   hierarchies, validating Islamic claim to Abrahamic legitimacy) while
 *   simultaneously extracting legitimacy from the Jewish exclusive-covenant
 *   claim that had grounded Jewish institutional authority. The constraint
 *   exhibits the characteristic properties of a kernel reading: it interprets
 *   a contested foundational text (the Genesis covenant promise) differently
 *   from sibling readings, generates beneficiaries and victims, and forces
 *   institutional authority structures to defend their interpretive choices
 *   against textual counter-arguments. The theater_ratio rises over the
 *   measurement interval (0.42 → 0.58) as traditional Jewish institutional
 *   defenses shift from textual exegesis to historical-sociological
 *   arguments, indicating increasing performative maintenance. The
 *   extractiveness also rises (0.28 → 0.52) as the reading gains scholarly
 *   credibility and becomes harder for exclusive-covenant authorities to
 *   marginalize.
 *
 * KEY AGENTS:
 *   - Jewish institutional authority (religious schools, rabbinical councils, institutional bodies): Primary victim — institutional legitimacy grounded in exclusive covenant interpretation is delegitimized
 *   - Traditional Jewish theological framework: Victim (collective) — claims covenantal exclusivity face textual contestation
 *   - Islamic community and Islamic theology: Primary beneficiary — gains Abrahamic genealogical legitimacy and theological validation
 *   - Interfaith dialogue institutions and leaders: Secondary beneficiary (organized) — benefit from enabling cross-faith theological frameworks
 *   - Inclusive abrahamic theologians: Mixed position — benefit from intellectual coherence and interfaith collaboration; constrained by career risk and institutional resistance
 *   - Analytical observers of theological systems: Risk of false-summit naturalization — could treat interpretive underdetermination as immutable rather than as contingent authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.52).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.48).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Abrahamic Covenant through Ishmael: Inclusive Reading of Genesis Promise").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'de4a50d8-de93-4dc3-85b3-e8fc1eef8097').
narrative_ontology:cs_kernel_codification('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', fixed_text).
narrative_ontology:cs_authority_grounding('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', lineage).
narrative_ontology:cs_interpretation_layer_present('de4a50d8-de93-4dc3-85b3-e8fc1eef8097').
narrative_ontology:cs_reading_relation('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', christian_supersessionist_reading, influences).
narrative_ontology:cs_axiom('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', foundational, covenant_extends_through_ishmael).
narrative_ontology:cs_axiom_status(covenant_extends_through_ishmael, holdable).
narrative_ontology:cs_axiom_grounding('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', covenant_extends_through_ishmael, empirically_contingent).
narrative_ontology:cs_axiom('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', secondary, multiple_valid_abrahamic_inheritances).
narrative_ontology:cs_axiom_status(multiple_valid_abrahamic_inheritances, holdable).
narrative_ontology:cs_axiom_grounding('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', multiple_valid_abrahamic_inheritances, conventional).
narrative_ontology:cs_reference_frame('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', inclusive_abrahamic_inheritance).
narrative_ontology:cs_drift_state('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', contemporary_interfaith_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de4a50d8-de93-4dc3-85b3-e8fc1eef8097', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_community).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, ishmael_descendants).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, inclusive_abrahamic_theologians).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_exclusive_covenant_claim).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, traditional_jewish_authority_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JEWISH EXCLUSIVE COVENANT CLAIM (SNARE) — The traditional reading of Genesis 17 as establishing exclusive covenant through Isaac faces an existential challenge from the Ishmael-inclusive reading. Jewish institutional authority that depends on exclusive Abrahamic legitimacy experiences this constraint as extractive suppression of their foundational claim. Identity-locked because the claim is constitutive of Jewish covenant theology itself; trapped because the suppression operates through counter-narration that cannot be escaped except by abandoning the theological framework. Maximum experienced extraction because the alternative reading directly contests the legitimacy of the exclusivity principle.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: ISLAMIC COMMUNITY / ISHMAEL INCLUSIVE READING (ROPE) — The reading that covenant extends through Ishmael to Muhammad provides Islamic theology with genealogical and textual legitimacy within the Abrahamic tradition. Institutional actors (Islamic leadership, theological schools) benefit from the inclusive framework, which enables cross-faith dialogue and positions Islam as continuing rather than superseding the Abrahamic promise. Arbitrage exit options: Islamic theology can adopt or minimize this reading depending on interfaith strategy. Experienced as coordination because the reading solves the problem of Abrahamic belonging without requiring Islam to claim supersession.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INCLUSIVE ABRAHAMIC THEOLOGIANS (TANGLED ROPE) — Scholars and religious leaders who advocate for the Ishmael-inclusive reading experience both coordination and extraction. Coordination function: the reading enables genuine inter-faith theological dialogue and challenges zero-sum legacy hierarchies. Extraction: advocacy for this reading carries career risk in traditional Jewish and Christian institutions, social ostracism from conservative communities, and resource constraints (publishing venues, funding, institutional position). Constrained exit: cannot easily leave the theological academy without abandoning their intellectual and vocational identity.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-FAITH COALITION BUILDING (TANGLED ROPE) — Organized religious leaders and institutions that use the Ishmael-inclusive reading to build interfaith councils, educational programs, and theological dialogues benefit from both coordination and modest extraction. Coordination: the reading genuinely enables shared theological frameworks and collaborative social action. Extraction: coalition leaders gain institutional authority, funding, and media prominence through being positioned as bridge-builders. Mobile exit: these organizations can shift framing or exit coalitions without existential cost, reducing suppression. Effective extraction is lower than victim perspective because the beneficiary has agency.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: TRADITIONAL JEWISH INSTITUTIONAL AUTHORITY / PITON VIEW (PITON) — The classical rabbinical reading of exclusive covenant through Isaac has maintained institutional authority through interpretive tradition (chain of transmission from Talmud through medieval commentaries to modern Orthodox authorities). The Ishmael-inclusive reading degrades this authority by showing that textual justification for exclusivity is contestable. The traditional reading persists through theatrical institutional maintenance (asserting textual closure, limiting hermeneutical challenge, marginalizing dissenting scholars) rather than through sustained argumentative force. Theater ratio is high: institutional authority must continuously reaffirm the exclusivity principle because the textual case is weaker than the institutional weight requires. This is a piton: a former mountain (claimed as natural law / eternal covenant) degraded into performative maintenance.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FALSE SUMMIT (MOUNTAIN) — From a civilizational vantage, one might argue that textual interpretation is inherently under-determined: the same biblical passage admits multiple consistent readings, and no reading can be derived as a logical necessity from the text alone. The text itself cannot adjudicate which reading is 'correct' — this is an immutable property of interpretation. However, this analysis naturalizes what is actually an institutional power structure: which readings are taught, published, funded, and canonized is determined by authority structures, not by the text's immanent properties. The engine's false summit detector will identify this as a naturalization of contingent institutional authority rather than as a genuine natural law.
constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abrahamic_covenant__ishmael_covenant_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, TR),
    TR >= 0.70.

:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. The reading extracts legitimacy from Jewish exclusive-covenant claims, but the extraction is partial rather than total — some Jewish scholars accept the inclusive reading, and Judaism has interpretive resources to accommodate multiple readings (textual pluralism is intrinsic to the Talmudic tradition). The rise from 0.28 to 0.52 reflects increasing scholarly visibility and institutional challenge; the reading becomes harder to suppress as interfaith scholarship produces alternative textual interpretations. Suppression (0.48): Moderate. Traditional Jewish institutions can and do suppress this reading through institutional controls (publishing gatekeeping, seminary curriculum decisions, relegating scholars to marginal positions), but suppression is incomplete because the reading has gained scholarly credibility and interfaith institutional backing. The reading cannot be completely expelled from the field. Theater ratio (0.58): Moderate-high and rising. As the inclusive reading becomes harder to suppress on textual grounds, Jewish institutional authority increasingly relies on theatrical assertions of textual closure and exclusivity rather than sustained exegetical argument. Traditional authorities reassert the exclusive reading through institutional authority weight rather than through textual demonstration. The rise from 0.42 to 0.58 signals increasing theater as the textual case weakens relative to institutional maintenance requirements.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish exclusive-covenant claim experiences this reading as maximum extraction (snare) because the constraint directly attacks the textual foundation of their institutional legitimacy. The Islamic community experiences it as coordination (rope) because it solves the structural problem of how Islam relates to Abrahamic inheritance. Interfaith theologians experience it as tangled_rope: genuine coordination benefit (enabling dialogue) mixed with career extraction (risk in traditional institutions). Traditional Jewish authority experiences it as piton degradation: the classical interpretation persists through institutional theater rather than through sustained argumentative force. The analytical observer risks seeing textual underdetermination as a natural law (mountain) — the text admits multiple readings, therefore interpretation is inherently under-determined — but this naturalizes what is actually an institutional authority structure that enforced exclusivity. The perspectival gap reveals that the constraint is not primarily about the text, but about institutional power to enforce one reading against others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies across perspectives based on the agent's structural relationship to the covenant legitimacy claim. Jewish institutional authority (exclusive covenant claim victim) experiences high d (~0.92): identity-locked to the exclusive reading, no exit options, maximum extraction burden. Islamic institutional community (inclusive reading beneficiary) experiences low d (~0.12): institutional beneficiary, arbitrage options, benefits from the constraint. Inclusive abrahamic theologians experience moderate d (~0.62): constrained exit (career risk), mixed benefits/costs, moderate extraction. The institutional authority structure naturally derives low χ for beneficiaries (negative effective extraction — they benefit from the constraint) and high χ for victims (trapped agents experience maximum extractiveness). The piton perspective derives moderate d (~0.55) because even though traditional authority formally maintains the exclusive reading, it does so through theater rather than through power — the institutional position is degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that it is a kernel reading, not a simple constraint classification problem. The mandatrophy question is: 'Is this reading a coordination mechanism or an extraction mechanism?' The answer is: it is both. The inclusive reading genuinely coordinates interfaith theology (solving the problem of how Islamic communities belong to Abrahamic inheritance) while simultaneously extracting from the Jewish exclusive-claim legitimacy (delegitimizing the institutional authority that depended on that claim). This is precisely what tangled_rope captures: genuine coordination function mixed with asymmetric extraction. The constraint does not resolve by picking one type — it resolves by recognizing that both the coordination and extraction are structurally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_underdetermination_vs_authority_choice,
    'Does the Ishmael-inclusive reading represent a genuine underdetermination of the biblical text, or is it an institutional choice enforced by authority structures?',
    'Hermeneutical analysis: if multiple readings are truly equipossible from the text, then textual underdetermination is the constraint; if the exclusive reading was privileged through institutional enforcement rather than textual necessity, then authority structures are the constraint',
    'If textual: the constraint is a mountain (immutable property of interpretation). If institutional: the constraint is tangled_rope or snare (contingent power structure). This determines whether the reading is contestable in principle or only in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_underdetermination_vs_authority_choice, conceptual, 'Whether textual underdetermination or institutional authority structures drive exclusive/inclusive reading choice').

omega_variable(
    kernel_reading_coexistence_paradox,
    'Can the exclusive (Isaac) and inclusive (Ishmael) readings of the covenant kernel coexist within a single Jewish or Christian framework, or does commitment to one reading foreclose the other?',
    'Theological analysis of framework-level commitments: Can a theologian hold both ''covenant is exclusively through Isaac'' AND ''covenant extends through Ishmael'' without internal contradiction? Or does commitment to one rule out the other?',
    'If coexistence is possible: readings relate as ''coexists_with'' (different communities hold different readings). If foreclosure occurs: readings relate as ''forecloses'' (one rules out the other). This determines the logical structure of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_paradox, conceptual, 'Logical coexistence or foreclosure between exclusive and inclusive covenant readings').

omega_variable(
    institutional_authority_grounding_shift,
    'As the Ishmael-inclusive reading gains scholarly and interfaith credibility, does Jewish institutional authority shift from claiming textual exclusivity to claiming historical exclusivity (emphasizing what Jews historically received vs. what the text permits)?',
    'Institutional analysis: track whether Jewish institutional responses to inclusive readings shift argumentative strategy from textual-exegetical to historical-sociological grounds',
    'If authority grounds shift: the constraint changes from textual interpretation (fixed) to historical narrative (more contingent). This indicates the constraint is degrading from mountain toward piton/tangled_rope as authority legitimacy depends on increasingly theater-heavy institutional performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_grounding_shift, empirical, 'Whether institutional authority shifts from textual to historical grounds as inclusive reading gains credibility').

omega_variable(
    beneficiary_set_asymmetry_in_extraction,
    'Does the Ishmael-inclusive reading function as a genuine coordination mechanism (solving the problem of Abrahamic belonging for multiple communities) or primarily as extraction from the Jewish exclusive-claim carrier?',
    'Structural analysis: If Islamic and interfaith communities genuinely solve coordination problems through this reading (reducing zero-sum competition, enabling joint action, creating stable multi-community frameworks), it is coordination. If the primary effect is delegitimizing Jewish exclusivity without building stable shared structures, it is extraction.',
    'If coordination-dominant: constraint is Rope. If extraction-dominant: constraint is Snare. If mixed: constraint is Tangled Rope. The empirical signature is whether adopting the inclusive reading produces net positive outcomes for all parties or redistributes legitimacy asymmetrically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_set_asymmetry_in_extraction, empirical, 'Whether inclusive reading functions as coordination or extraction relative to beneficiary set').

omega_variable(
    reading_status_at_kernel_boundary,
    'Is this reading one of multiple competing interpretations of a stable kernel, or does this reading claim to reveal that the kernel itself (as traditionally understood) was a contingent institutional construction?',
    'Theological-philosophical analysis: Does the inclusive reading interpret ''covenant'' within traditional boundaries, or does it argue that ''covenant'' was never the right frame to begin with?',
    'If interpreting within boundaries: the reading is a perspectival alternative (coexists_with other readings). If claiming the kernel itself is false: the reading influences or forecloses other readings by undermining their shared foundation. This determines whether the kernel_reading relationship is interpretive diversity or structural contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_status_at_kernel_boundary, conceptual, 'Whether reading interprets kernel or contests kernel''s legitimacy itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahcov_ish_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(abrahcov_ish_tr_t3, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(abrahcov_ish_tr_t6, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(abrahcov_ish_tr_t9, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(abrahcov_ish_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(abrahcov_ish_be_t3, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(abrahcov_ish_be_t6, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(abrahcov_ish_be_t9, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 9, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(abrahcov_ish_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(abrahcov_ish_su_t6, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__ishmael_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, jewish_institutional_legitimacy__textual_authority).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, islamic_community__abrahamic_belonging).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, interfaith_dialogue__institutional_authority).

% DUAL FORMULATION NOTE:
% The Ishmael-inclusive reading is one reading of the covenant kernel. The Isaac-exclusive reading and Christian supersessionist reading are separate constraints with different ε values and beneficiary/victim structures. All three readings should be modeled as distinct constraint stories linked via network.affects_constraints, not as a single constraint with 'multiple interpretations.' The covenant kernel is the stabilized commitment (Genesis 17); the readings are competing claims about how the kernel applies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
