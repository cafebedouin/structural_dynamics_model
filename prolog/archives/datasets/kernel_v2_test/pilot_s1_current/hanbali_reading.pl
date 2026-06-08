% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Textualist Jurisprudence: Primacy of Qur'an/Sunnah with Minimal Analogical Extension
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Hanbali reading of Islamic jurisprudential method institutes a
 *   constraint on legal reasoning by privileging the textual corpus (Qur'an
 *   and authenticated Sunnah) and imposing strict limits on analogical
 *   extension (qiyas) and rationalist inference. This reading is one of four
 *   major interpretations of the shared jurisprudential_method_kernel — the
 *   commitment to ground Islamic law in authoritative sources. The Hanbali
 *   reading narrows the scope of derived rulings by treating the textual
 *   corpus as epistemically closed and by suspecting rationalist methods as
 *   internally grounded rather than externally anchored. The constraint
 *   operates simultaneously as a coordination mechanism (clarifying
 *   boundaries for legitimate juristic authority), an extraction mechanism
 *   (beneficiaries: textual literalist scholars and hadith specialists;
 *   victims: jurists employing rationalist methods and those addressing novel
 *   circumstances), a degraded institution (state enforcement reducing active
 *   juristic innovation to ceremonial invocation), and a false summit
 *   (naturalizing a contingent institutional commitment as an epistemic law).
 *   The extractiveness trajectory is stable (0.28→0.35) reflecting sustained
 *   institutional pressure, while theater_ratio rises (0.20→0.38) as
 *   jurisdictions relying on the official reading increasingly employ
 *   alternative mechanisms (administrative law, state decrees) while
 *   maintaining symbolic fidelity to the framework.
 *
 * KEY AGENTS:
 *   - Textual Literalist Scholars: Primary beneficiary (institutional/arbitrage) — elevated status as guardians of methodological authenticity
 *   - Hadith Transmission Community: Primary beneficiary (institutional/arbitrage) — textual primacy axiom elevates hadith study and authentication
 *   - Jurists Employing Rationalist Methods: Primary victim (moderate/constrained) — suppressed methodological scope; constrained rather than trapped because alternative schools maintain rationalist traditions
 *   - Novel Circumstance Claimants: Secondary victim (powerless/identity_locked) — those seeking rulings on unprecedented situations face an identity lock: they must either abandon the school's framework or perform costly reinterpretation
 *   - Court Administrators (in jurisdictions with official Hanbali madhab): Mixed position (moderate/constrained) — benefit from clarity but bear costs of inflexibility
 *   - State Religious Authority: Institutional actor (institutional/constrained) — maintains official symbolic commitment while enforcing alternative legal mechanisms
 *   - Modern Reform Movements: Organized actors (organized/mobile) — revive the reading as part of authenticity project; see it as temporarily restrictive scaffold
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing constructed commitment as epistemic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.35).
domain_priors:suppression_score(hanbali_reading, 0.48).
domain_priors:theater_ratio(hanbali_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Textualist Jurisprudence: Primacy of Qur'an/Sunnah with Minimal Analogical Extension").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, '2c8a794a-0043-4f53-8a11-b6ec9ce2b66b').
narrative_ontology:cs_kernel_codification('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', fixed_text).
narrative_ontology:cs_authority_grounding('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', lineage).
narrative_ontology:cs_interpretation_layer_present('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b').
narrative_ontology:cs_reading_relation('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', hanbali_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', hanbali_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', hanbali_reading__shafi_i_reading, coexists_with).
narrative_ontology:cs_axiom('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', foundational, textual_primacy_with_restricted_extension).
narrative_ontology:cs_axiom_status(textual_primacy_with_restricted_extension, holdable).
narrative_ontology:cs_axiom_grounding('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', textual_primacy_with_restricted_extension, empirically_contingent).
narrative_ontology:cs_axiom('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', foundational, epistemic_suspicion_of_rationalist_grounding).
narrative_ontology:cs_axiom_status(epistemic_suspicion_of_rationalist_grounding, holdable).
narrative_ontology:cs_axiom_grounding('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', epistemic_suspicion_of_rationalist_grounding, deontological).
narrative_ontology:cs_reference_frame('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', classical_hadith_centered_jurisprudence).
narrative_ontology:cs_drift_state('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', contemporary_modern_administration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2c8a794a-0043-4f53-8a11-b6ec9ce2b66b', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, textual_literalist_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, traditionalist_legal_authority).
narrative_ontology:constraint_beneficiary(hanbali_reading, hadith_transmission_community).
narrative_ontology:constraint_victim(hanbali_reading, jurists_employing_rationalist_methods).
narrative_ontology:constraint_victim(hanbali_reading, novel_circumstance_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JURISPRUDENT SEEKING NOVEL RULINGS (SNARE) — A scholar attempting to address a circumstance not directly covered by Qur'an or Sunnah faces a structural trap. The Hanbali reading forecloses the most straightforward analogical reasoning (qiyas) available in other schools. The scholar must either (1) suppress the novel circumstance as outside jurisprudence's scope, (2) perform costly linguistic gymnastics to force a textual precedent, or (3) abandon the school's framework. The exit is costly in professional identity, fiqh standing, and scholarly coherence. Identity-locked because the scholar's authority as a Hanbali jurist is constituted through fidelity to the school's method — exit requires becoming a different type of legal scholar. Maximum extraction from this position.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: COURT ADMINISTRATOR APPLYING HANBALI LAW (TANGLED ROPE) — State religious courts in some jurisdictions (historical Ottoman, contemporary Saudi Arabia) adopt Hanbali methodology as official doctrine. Administrators benefit from the school's high evidentiary bar (claims rooted only in preserved texts reduce litigation over interpretive disputes) and from the coordination function (clear boundaries for juristic authority). But they also bear costs: novel circumstances require expensive scholarly consultations, rulings appear inconsistent when the text is silent, and the constraint's suppression (narrow methodological permission) limits administrative flexibility. Mixed coordination and extraction — genuine legal coordination alongside institutional restriction.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HADITH TRANSMISSION COMMUNITY (ROPE) — Scholars specialized in hadith collection, authentication, and transmission benefit substantially from the Hanbali framework's emphasis on textual fidelity. The reading elevates hadith study from one juristic tool among many to THE primary evidentiary base. Hadith scholars see the Hanbali method as solving a genuine coordination problem: how to ground law in preserved sources rather than in jurists' rational reconstructions. This group has arbitrage — they can engage other schools' methodologies but benefit from this one's primacy assumption. Net beneficiary; experiences the constraint as enabling coordination.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERN ISLAMIC REFORM MOVEMENTS (SCAFFOLD) — Contemporary reform movements (Salafi, pietist, neo-traditionalist) have revived and extended the Hanbali framework as part of a broader project to return to 'authentic Islam' stripped of centuries of philosophical accretion. These movements adopt the textualist methodology as a transitional mechanism: they claim it will purify jurisprudence and restore direct engagement with foundational texts. The scaffold perspective sees this as temporary — a methodological position meant to clear away rationalist overgrowth and establish fresh interpretation. Mobile exit because these movements can migrate to other schools or develop their own syntheses. The sunset is implicit: once 'authenticity' is recovered, the temporary restriction may relax.
constraint_indexing:constraint_classification(hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE RELIGIOUS AUTHORITY (PITON) — In jurisdictions where the state enforces a particular madhab (legal school) as official doctrine, the Hanbali framework becomes a performative commitment maintained through institutional inertia and legitimacy claims rather than through active juristic innovation. The school's original function (providing methodological clarity through textual restriction) persists as a symbolic commitment even when state administrators rely on creative reinterpretation or parallel legal mechanisms (administrative law, royal decrees) to address novel circumstances. Theater high because official doctrine is invoked ceremonially while substantive novel rulings proceed through alternative channels. Constrained exit because state religious authority is constitutionally locked into the school.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: RATIONALIST LEGAL SCHOLARS (TANGLED ROPE) — Philosophers and jurists employing rationalist methods (Mu'tazila-influenced, Ash'arite philosophical theology, modern legal philosophy) benefit from a genuine coordination function: the Hanbali framework's restriction provides a clear boundary against which to define their own methodology. They can sharply delineate what rationalist jurisprudence does differently. But they also bear substantial costs: the dominant institutional position of the Hanbali framework (or related textualist schools in particular regions) suppresses their methods, limits their scholarly influence, and constrains the scope of rational legal inference they can publicly advocate. Constrained rather than trapped because philosophical communities have maintained alternative schools and lineages. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an analytical/civilizational perspective, the textualist commitment to a fixed corpus (Qur'an and authenticated Sunnah) and suspicion of rational extension emerges as a natural law of epistemology: once the authoritative sources are fixed, any inference beyond them must be externally grounded, and rationalist methods (being internal to the jurist's mind rather than rooted in preserved texts) appear as unstable ground for law. This perspective naturalizes the methodological restriction as an immutable epistemic principle. However, the structural data (beneficiaries, institutional enforcement, suppression of alternatives) suggests a false summit: what appears as a natural epistemic law is actually a constructed commitment system maintained by actors who benefit from it. The engine's false summit detector will flag this as a naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts value from rationalist jurists by suppressing their methodological scope and from novel circumstance seekers by forcing costly reinterpretation or framework abandonment. But the extraction is not maximal (0.72 would indicate pure predation) because textual scholars genuinely coordinate through shared methodological boundaries, and hadith specialists legitimately benefit from elevated evidentiary standards. The beneficiary group is real and the coordination function is genuine — this is tangled_rope, not snare. Suppression (0.48): Moderate-high. The constraint suppresses alternative methodologies through institutional authority, state enforcement (in some jurisdictions), and epistemic delegitimation (rationalist methods presented as unstable ground). But suppression is not total because other schools remain institutionalized, hadith critics engage in sophisticated logical reasoning even while denying they do, and modern movements can reframe the methodology for different purposes. Theater ratio (0.20 initially, rising to 0.38): Low-moderate, increasing. The Hanbali framework was originally highly functional — it provided genuine epistemic clarity. As novel circumstances accumulated (historical empire administration, modern governance, contemporary biomedical questions), the restriction's rigidity became visible. State authorities increasingly use alternative legal mechanisms (administrative orders, royal decrees, regulatory committees) while maintaining ceremonial invocation of Hanbali principle, driving the theater ratio upward. The rising trajectory reflects the constraint's function atrophying relative to its institutional maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This reading exhibits strong perspectival gaps characteristic of tangled_rope constraints. The textual scholar sees coordination and clarity (rope perspective). The hadith specialist sees elevation and enabling (rope perspective). The rationalist jurist sees suppression and extraction (snare or tangled_rope perspective). The novel circumstance seeker sees a trap (snare). The court administrator sees mixed coordination and constraint (tangled_rope). The state religious authority sees degraded function maintained ceremonially (piton). The reform movement sees temporary purification en route to authenticity (scaffold). The analytical observer risks seeing an epistemic law (mountain/false summit). The perspectival spread (mountain through snare) reveals a constraint whose type is not intrinsic but observer-relative — the same base properties generate six different classifications depending on the agent's power, time horizon, exit options, and scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim status, power level, exit options, and scope. Textual scholars (beneficiary, institutional, arbitrage): d ≈ 0.1 (low directionality; high beneficiary status, easy exit → negative effective extraction). Hadith specialists (beneficiary, institutional, arbitrage): d ≈ 0.15. Rationalist jurists (victim, moderate, constrained): d ≈ 0.65 (high directionality; victim status, constrained exit → high effective extraction). Novel circumstance seekers (victim, powerless, identity_locked): d ≈ 0.85 (maximum directionality; victim status, identity lock → maximum experienced extraction). Court administrators (mixed, moderate, constrained): d ≈ 0.50 (symmetric; coordination benefits offset constraint costs). The engine applies f(d) (sigmoid directionality function) to convert these to effective extractiveness (χ), which varies by scope and institutional coupling. The derived directional asymmetry confirms tangled_rope classification: beneficiaries experience low/negative χ, victims experience high χ, from the same base constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate was to provide methodological clarity: define legitimate grounds for juristic inference so that law could be grounded in preserved sources rather than in individual jurists' rational reconstructions. That mandate remained live for centuries in the context of manuscript-based legal transmission and oral scholarly communities where method mattered deeply. The mandate is now contested because: (1) state institutions (late Ottoman, contemporary kingdoms) employ Hanbali authority while using alternative mechanisms (administrative law, royal decrees) for actual novel rulings, creating a gap between official doctrine and operational law; (2) modern jurisdictions have developed parallel legal systems (civil codes, regulatory frameworks, specialized courts) that function independently of classical juristic methodology; (3) the textual corpus has not grown while circumstances have expanded, making the original promise of direct textual grounding impossible to sustain. The theater_ratio rise (0.20→0.38) tracks this mandate-function gap. Mandatrophy is NOT fully resolved — the reading still structures legitimate scholarly debate, still provides coherent methodological boundaries, and is still invoked in public discourse — but its operational function has substantially attenuated. Classification as tangled_rope rather than piton reflects that genuine coordination and extraction are still live; the constraint is not yet purely theatrical. If theater_ratio continues rising above 0.50, reclassification toward piton would be justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_vs_interpretive_evolution,
    'Is the Qur''an and hadith corpus genuinely fixed and closed (allowing only retrieval of preserved meanings), or does engagement with the text across generations constitute an evolving interpretive corpus that legitimately develops new readings?',
    'Historical analysis of hadith authentication standards, collection codification timelines, and the treatment of textual variants and abrogation. Does the Hanbali framework''s claim of epistemic closure rest on historical fact (sources are genuinely complete and fixed) or on a methodological commitment (treating them as if fixed)?',
    'If sources are genuinely closed: the textualist methodology is epistemically justified; classification remains tangled_rope (organizational/beneficiary asymmetry), not false summit. If sources are treated as fixed through methodological choice: the mountain perspective is a false summit; the constraint is a constructed institutional commitment, not a law of nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fixity_vs_interpretive_evolution, empirical, 'Whether textual closure is epistemic fact or methodological commitment').

omega_variable(
    analogy_foreclosure_necessity,
    'Does the Hanbali restriction on analogical extension (qiyas) logically follow from the textual primacy axiom, or is it an additional normative commitment that could coexist with textual primacy while permitting broader analogical reasoning?',
    'Jurisprudential analysis comparing Hanbali texts with other schools'' texts on the relationship between textual primacy and the scope of analogy. Can a school affirm the primacy of Qur''an and Sunnah while employing Hanafi-style broad qiyas? Or does textual primacy logically entail the Hanbali restriction?',
    'If restriction logically entails from primacy: Hanbali and Hanafi readings FORECLOSE each other''s core methodologies (rare relationship). If restriction is additional choice: readings COEXIST_WITH each other under shared textual primacy commitment but differ on downstream implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogy_foreclosure_necessity, conceptual, 'Whether analogy restriction logically follows from textual primacy').

omega_variable(
    rationalist_method_coherence,
    'Can the Hanbali framework''s suspicion of rational extension coherently rule out rationalist juristic methods, or does the framework itself employ reason (in authentication standards, in drawing implications from texts, in handling textual conflicts) such that the distinction between ''textual'' and ''rationalist'' methods is itself unstable?',
    'Detailed examination of Hanbali legal reasoning in practice: does it employ reason only instrumentally (to apply preserved texts) or substantively (to extend meanings)? Are there points where Hanbali scholars perform sophisticated logical inference that might appear rationalist if reframed?',
    'If the distinction is stable: the restriction coherently forecloses rationalist methods. If the distinction is unstable: the framework is performative (theater_ratio concern); Hanbali scholars practice rationalist jurisprudence while denying it in theory. Feeds classification uncertainty between tangled_rope and piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_method_coherence, conceptual, 'Whether Hanbali framework coherently distinguishes textual from rationalist methods').

omega_variable(
    contemporary_institutional_closure,
    'In modern jurisdictions where Hanbali jurisprudence is institutionally privileged (state religious authority, official madhab), to what extent is the suppression of rationalist and alternative methods a consequence of the Hanbali axioms themselves versus a consequence of state enforcement?',
    'Comparison of suppression levels in jurisdictions where Hanbali is dominant institutional teaching (high enforcement) versus jurisdictions where it coexists with other schools (no state enforcement). If suppression correlates with state enforcement rather than with the methodology itself, the constraint''s extractiveness is partially derivative from institutional power, not from the reading''s internal logic.',
    'If internal logic drives suppression: the constraint is a pure reflection of methodological commitments (perhaps legitimately restricting false inference). If state enforcement drives suppression: the constraint is substantially extractive through institutional asymmetry — classification shifts toward snare. Determines whether beneficiary extraction is inherent to the reading or contingent on state backing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_institutional_closure, empirical, 'Whether suppression follows from methodology or from institutional enforcement').

omega_variable(
    kernel_reading_alternative_interpretations,
    'Does the jurisprudential_method_kernel permit coherent alternative readings of textual primacy that do NOT entail the Hanbali-specific restrictions on analogy, or is the Hanbali reading the only defensible interpretation of the kernel''s structure?',
    'Analysis of the other madhabs'' texts on textual primacy and their justifications for broader analogical extension. Do they claim equal textual fidelity while disagreeing on methodology? Or do they adopt different kernels entirely?',
    'If alternatives are coherent readings of the same kernel: this reading COEXISTS_WITH siblings under a shared commitment system. If alternatives require different kernels: FORECLOSE relationship possible. Determines whether the contested kernel structure permits the perspectival pluralism the DR system models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_interpretations, conceptual, 'Whether alternative madhabs read the same jurisprudential kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanb_tr_t0, hanbali_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hanb_tr_t3, hanbali_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(hanb_tr_t6, hanbali_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(hanb_tr_t10, hanbali_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(hanb_be_t0, hanbali_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hanb_be_t3, hanbali_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(hanb_be_t6, hanbali_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(hanb_be_t10, hanbali_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanb_su_t0, hanbali_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hanb_su_t5, hanbali_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(hanb_su_t10, hanbali_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hanbali_reading, 0.12).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafi_i_reading).
narrative_ontology:affects_constraint(hanbali_reading, classical_hadith_transmission_authority).
narrative_ontology:affects_constraint(hanbali_reading, islamic_state_legal_system).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one of four constraint stories sharing the jurisprudential_method_kernel. Each reading has its own ε value (extractiveness), beneficiary/victim structure, and theater trajectory. They are related not as observable variants of one constraint but as structurally distinct constraints instantiated from the same contested kernel. The Hanbali reading's ε=0.35 reflects its particular balance of genuine methodological coordination and substantive suppression of alternatives. Hanafi, Maliki, and Shafi'i readings would have different ε values reflecting their different benefits and costs. All four share the upstream kernel constraint (commitment to ground law in authoritative sources), which has low ε (high coordination, minimal extraction). The downstream constraint (authority of particular scholarly interpretations) is a separate story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanbali_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
