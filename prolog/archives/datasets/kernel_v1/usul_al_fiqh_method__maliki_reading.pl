% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Reading of Islamic Legal Methodology (usul al-fiqh): Practice-Norm and Public Interest Authority
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Maliki school of Islamic jurisprudence (usul al-fiqh) represents one
 *   major reading of how Islamic law should be derived and applied. This
 *   reading distinguishes itself by granting authoritative status to Medinan
 *   practice ('amal ahl al-Madina) — the documented customs and legal
 *   decisions of the Prophet's city in the early Islamic period — as an
 *   interpretive layer that validates, contextualizes, or sometimes overrides
 *   hadith (prophetic tradition) in ambiguous cases. Additionally, the Maliki
 *   reading incorporates maslaha (public interest or the common good) as a
 *   standalone source of juristic reasoning when explicit texts do not
 *   address a situation. These two innovations — practice-authority and
 *   public-interest reasoning — create a constraint that benefits Maliki
 *   jurists (who gain interpretive power to determine which practices and
 *   interests count as legitimate) while constraining universal textualist
 *   approaches that would derive law primarily from explicit hadith
 *   transmission. The constraint exhibits genuine coordination function: it
 *   provides a stable, systematic method for deriving law that respects both
 *   textual sources and community practice, enabling legal consistency across
 *   generations and regions. It also exhibits extraction: the authority to
 *   interpret 'amal and maslaha is concentrated in Maliki jurists, who can
 *   use these tools to favor locally-rooted legal positions, resist
 *   standardization from other madhabs, and preserve regional customs against
 *   universalizing pressure. The constraint is neither pure coordination (a
 *   rope) nor pure extraction (a snare) — it is a tangled hybrid (tangled
 *   rope) where genuine methodological function and institutional extraction
 *   are structurally inseparable.
 *
 * KEY AGENTS:
 *   - Maliki Jurists (institutional/constrained): Primary beneficiaries of interpretive authority; establish themselves as arbiters of legitimate practice and public interest; constrained by need to maintain coherence within Maliki tradition
 *   - Local Custom-Preserving Communities (powerless/trapped): Bear the cost of having their practices adjudicated by Maliki authorities; cannot exit to alternative legal systems without loss of legitimacy or institutional protection
 *   - Textualist Hadith Scholars (institutional/arbitrage): Enjoy mobility through hadith networks; experience constraint as coordination rather than extraction; not trapped by Maliki methodology
 *   - Political Authority/Rulers (powerful/mobile): Experience mixed coordination (stable legal framework) and extraction (judicial veto by Maliki jurists); can theoretically switch madhabs but risk legitimacy loss
 *   - Reform-Minded Modernizing Jurists (organized/constrained): See maslaha as a built-in exit mechanism permitting reinterpretation as conditions change; experience the constraint as temporary (scaffold) rather than permanent
 *   - Historical Institutional Inertia (institutional/constrained): The civilizational view reveals that the methodological innovation (practice-authority, maslaha reasoning) has become largely performative as material conditions change; modern courts invoke Medinian practice through textual interpretation rather than living custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Reading of Islamic Legal Methodology (usul al-fiqh): Practice-Norm and Public Interest Authority").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '19359c14-aa80-48fe-9b1b-12e529912096').
narrative_ontology:cs_kernel_codification('19359c14-aa80-48fe-9b1b-12e529912096', distributed).
narrative_ontology:cs_authority_grounding('19359c14-aa80-48fe-9b1b-12e529912096', lineage).
narrative_ontology:cs_interpretation_layer_present('19359c14-aa80-48fe-9b1b-12e529912096').
narrative_ontology:cs_reading_relation('19359c14-aa80-48fe-9b1b-12e529912096', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('19359c14-aa80-48fe-9b1b-12e529912096', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('19359c14-aa80-48fe-9b1b-12e529912096', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('19359c14-aa80-48fe-9b1b-12e529912096', foundational, medinian_practice_authoritative_source).
narrative_ontology:cs_axiom_status(medinian_practice_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('19359c14-aa80-48fe-9b1b-12e529912096', medinian_practice_authoritative_source, empirically_contingent).
narrative_ontology:cs_axiom('19359c14-aa80-48fe-9b1b-12e529912096', foundational, maslaha_independent_legitimate_source).
narrative_ontology:cs_axiom_status(maslaha_independent_legitimate_source, holdable).
narrative_ontology:cs_axiom_grounding('19359c14-aa80-48fe-9b1b-12e529912096', maslaha_independent_legitimate_source, deontological).
narrative_ontology:cs_reference_frame('19359c14-aa80-48fe-9b1b-12e529912096', medinian_practice_living_authority).
narrative_ontology:cs_drift_state('19359c14-aa80-48fe-9b1b-12e529912096', post_hadith_compilation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19359c14-aa80-48fe-9b1b-12e529912096', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_custom_preservation).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, practice_based_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_uniformity).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_medinian_legal_traditions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universal_hadith_hierarchy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MALIKI JURIST ESTABLISHMENT (TANGLED ROPE) — Institutional actors embedded in Maliki legal tradition experience genuine coordination function (anchoring jurisprudence to Medinian practice enables stable legal reasoning across generations and regions) alongside extraction benefit (authority to adjudicate which customs count as 'amal ahl al-Madina and which maslaha claims are legitimate). Constrained exit: breaking from the tradition risks professional legitimacy and institutional standing. The constraint both enables their legal reasoning and grants them interpretive power.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-MALIKI REGIONAL COMMUNITIES (SNARE) — Local communities in regions where Maliki jurisprudence is authoritative (Maghreb, West Africa, parts of al-Andalus historically) are bound by Maliki interpretation of 'amal and maslaha without meaningful exit options. They cannot easily adopt a different madhab; institutional pressure and social convention enforce Maliki authority. The extraction flows: Maliki jurists gain interpretive control; local communities bear the cost of having their own customs either validated (if advantageous to Maliki interests) or suppressed (if deemed contrary to maslaha as Maliki authorities define it). High suppression: alternative legal frameworks are delegitimized.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: TEXTUALIST HADITH SCHOLARS (ROPE) — Scholars committed to explicit hadith transmission enjoy mobility through the global hadith network and can arbitrage between madhabs. The Maliki constraint does not trap them; they can work within Maliki methodology while maintaining access to the broader hadith corpus. They experience the constraint as coordination (standardizing which hadith count as authoritative within Maliki methodology) rather than extraction. The relationship is symmetric: hadith scholars contribute canonical texts; Maliki methodology integrates them into a coherent system.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLITICAL AUTHORITY (TANGLED ROPE) — Rulers who adopt Maliki jurisprudence experience both coordination and extraction. Coordination: the methodology provides stable legal framework for governance across diverse local customs. Extraction: Maliki jurists gain veto power over law through maslaha determinations, and rulers are constrained by the established authority of Medinian practice. Mobile exit: rulers can theoretically switch madhabs or establish their own legal council, but doing so risks legitimacy. The constraint is mixed coordination-extraction with moderate experienced extraction.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORM-MINDED JURISTS (SCAFFOLD) — Progressive legal scholars seeking to adapt Islamic jurisprudence to modern conditions experience the Maliki methodology as a temporary constraint with a sunset. The appeal to maslaha (public interest) offers a built-in mechanism for updating methodology: if contemporary conditions change what counts as public interest, the framework itself permits reinterpretation. Sunset logic: as social consensus shifts (modern nation-states, industrial economies, global communication), the authoritative weight of medieval Medinian practice naturally declines. Low effective extraction because the constraint contains its own exit mechanism through maslaha reasoning.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: HISTORICAL INSTITUTIONAL INERTIA (PITON) — From the civilizational/global perspective, Maliki jurisprudence in the modern Islamic world persists largely through institutional inheritance and identity attachment rather than active verification of its methodological superiority. The ritual of citing 'amal ahl al-Madina continues in judicial decisions, but the material conditions that made this methodology distinctive (Medinian practice as recent oral tradition) have long since passed. Theater ratio: high — modern Maliki courts invoke Medinian practice in language but apply it through textual interpretation rather than living custom. The constraint persists through inertia; the primary function (stable legal reasoning grounded in authentic practice) has atrophied.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The cross-madhab analytical perspective reveals that the Maliki reading of usul al-fiqh is one of four structurally distinct methodological commitments, each with identifiable beneficiaries and victims. The constraint exhibits genuine coordination function (providing stable legal reasoning framework) alongside extraction asymmetry (institutional authority concentrated in Maliki jurists, with power to determine which customs and interests count as legitimate). The extraction is moderate and subject to contestation by sibling readings, which is why it classifies as tangled rope rather than snare at the analytical level.
constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method__maliki_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, TR),
    TR >= 0.70.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The Maliki reading grants jurists genuine interpretive authority over 'amal (which practices count as authoritative Medinian precedent) and maslaha (what constitutes legitimate public interest). This is real power. However, the extraction is constrained by: (1) the need to justify decisions within the Maliki tradition, maintaining internal coherence; (2) competition from three other major madhabs that offer alternative readings of usul al-fiqh, limiting Maliki jurists' ability to enforce their methodology globally; (3) the inherent ambiguity of both 'amal and maslaha, which permits ongoing contestation and reinterpretation by later jurists; (4) the moderating effect that maslaha contains an ostensible appeal to common good rather than pure jurist preference. If the methodology were purely extractive (like a snare), we would expect Maliki jurists to invoke 'amal and maslaha to consistently favor their institutional interests; the empirical record shows more variation and genuine constraint by textual commitments. Suppression (0.48): Moderate-high. Non-Maliki regions under Maliki jurisdiction face significant barriers to adopting alternative legal frameworks. Suppression rises over the interval (0.35 → 0.48) as Maliki institutional structures consolidate and alternative madhabs become harder to practice openly. However, suppression is not total: hadith scholars maintain parallel authority; some regions preserve Shafi'i or Hanafi jurisprudence even under nominal Maliki dominance; the methodology itself (unlike pure ideology) is contestable on rational grounds. Theater ratio (0.35): Moderate-low. The Maliki methodology, especially in its early formulation (Malik's Muwatta), reflects genuine engagement with documented practice and reasoned juristic argument. It is not primarily performative. However, as the interval progresses, theater rises (0.15 → 0.38) because the material conditions that made Medinian practice recent and verifiable have disappeared; by the Ottoman period, invoking 'amal ahl al-Madina is largely textual citation rather than engagement with living practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's nature as tangled rope. Maliki jurists (institutional/constrained) experience it as rope — genuine coordination that enables legal reasoning. Non-Maliki communities under Maliki jurisdiction (powerless/trapped) experience it as snare — they are bound without exit. Rulers (powerful/mobile) experience mixed extraction and coordination. Hadith scholars (institutional/arbitrage) experience it as rope — they can move between frameworks. Modernizing jurists (organized/constrained) experience it as scaffold — maslaha offers a sunset mechanism. The analytical observer (analytical/analytical) sees the constraint as structurally containing both real coordination and real extraction, properly classified as tangled rope. The fact that all perspectives do not converge on the same type (some see rope, some see snare, some see scaffold) indicates the constraint is genuinely hybrid: if it were pure rope, all observers would see coordination; if it were pure snare, all would see extraction. The perspectival diversity itself is diagnostic of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: Maliki jurists are beneficiaries with constrained exit (institutional power but cannot easily abandon the tradition they benefit from) → moderate d → moderate chi. Local communities are victims with trapped exit (powerless, no alternative legal recourse) → high d → high f(d) → high experienced extraction. Hadith scholars are beneficiaries with arbitrage exit (institutional power and can move between frameworks) → low d → negative f(d) → low or negative chi (they experience subsidy rather than extraction). Rulers are ambiguous (both beneficiaries from the coordination function and victims from the judicial veto by jurists) → moderate d. Reform jurists are organized (can coordinate alternative interpretations) with constrained exit (cannot abandon Islamic jurisprudence framework) → moderate d. The piton perspective represents a global/civilizational view where the original coordinative function has degraded due to changed conditions → moderate d but with high theater masking the atrophy. The analytical perspective measures the average or aggregate directionality across all positions → moderate d reflecting the true hybrid nature.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING ANALYSIS: This constraint instantiates the Maliki reading of the usul al-fiqh (Islamic legal methodology) kernel. The mandatrophy — 'Is this genuine coordination or disguised extraction?' — is resolved by recognizing that the Maliki reading IS contestable coordination-with-extraction, standing alongside three alternative readings (Hanafi, Shafi'i, Hanbali) that offer different solutions to the same problem: how should Islamic law be derived when explicit textual sources are insufficient? The Maliki answer (via practice-authority and maslaha) is one of four coherent, internally consistent methodologies. The 'extraction' component arises because committing to this particular methodology grants Maliki jurists interpretive power. But the coordination component is real: the methodology enables stable, predictable legal reasoning. The constraint is correctly classified as tangled rope because both components are structural, neither masks the other, and both are present in the architecture of the methodology itself. The false summit risk would be if we tried to classify this as mountain (an immutable law of Islamic jurisprudence) or as pure snare (pure extraction masquerading as law). The indexed perspectives prevent both errors: the methodological commitment is perspectival (different madhabs exist), and the extraction is real but not totalizing (other madhabs compete; reform mechanisms exist; alternative framings are possible).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amal_ahl_al_madina_authenticity,
    'Which practices actually constitute authentic ''amal ahl al-Madina, and does the practice-authority rest on historical verification or on Maliki juristic consensus about what counts as authoritative practice?',
    'Historical reconstruction of documented Medinian practices in the first two centuries; comparison with actual Maliki juristic rulings to identify gaps and reversals; analysis of when and why Maliki authorities rejected specific practices despite strong historical attestation',
    'If authority rests on authentic historical reconstruction: maslaha becomes a secondary layer, and Maliki methodology remains anchored to verifiable practice. If authority rests on juristic consensus about practice: the distinction between ''amal and qiyas collapses, and the Maliki reading converges toward the Hanafi approach (expanding juristic discretion). Classification would shift toward higher extractiveness if the authority is juristic rather than historical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amal_ahl_al_madina_authenticity, empirical, 'Authenticity and determination of ''amal ahl al-Madina').

omega_variable(
    maslaha_scope_and_limits,
    'What boundaries constrain maslaha as a legitimate source? Can maslaha override explicit hadith, or only fill gaps where texts are silent? Does maslaha require explicit textual anchor, or can it stand alone?',
    'Systematic analysis of Maliki juristic texts (Mudawwana, Risala, Muwatta commentary) documenting cases where maslaha was invoked; categorization of how often maslaha outcomes conflict with explicit hadith vs. silence; study of later Maliki scholars'' treatment of al-Shatibi''s universalization of maslaha',
    'If maslaha is tightly constrained (must have textual anchor, cannot override hadith): Maliki reading remains moderate on extractiveness and closer to Shafi''i systematicity. If maslaha is broadly exercised (can override hadith, requires only juristic consensus on public interest): extractiveness rises substantially and the methodology becomes nearly equivalent to Hanafi istihsan (juristic preference) — converging toward Hanafi rather than remaining distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maslaha_scope_and_limits, empirical, 'Boundaries and scope of maslaha as a legitimate legal source').

omega_variable(
    regional_variance_vs_unified_madhab,
    'Is Maliki jurisprudence experienced as a unified methodological framework across Maliki-majority regions, or as a collection of regional juristic schools with significant local variation?',
    'Comparative analysis of Maliki juristic rulings across Maghreb, al-Andalus, Egypt, and East Africa; documentation of how often local Maliki authorities reached different conclusions on the same issue based on local ''amal and maslaha determinations; study of whether centralized Maliki institutions (like Al-Qarawiyyin) enforced methodological uniformity or permitted local variation',
    'If experienced as unified: Maliki reading is a genuine constraint producing institutional coherence. If experienced as regional variation: the apparent constraint is actually fragmented, and extractiveness classification depends on how local jurists experienced the Maliki authority claims — possibly lower if local communities had genuine discretion in interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variance_vs_unified_madhab, empirical, 'Degree of regional variation vs. unified methodological framework').

omega_variable(
    kernel_vs_reading_ambiguity,
    'Is this constraint the Maliki reading of a contested kernel (usul al-fiqh methodology), or is it a description of Maliki jurisprudence as a distinct methodological system claiming its own authority?',
    'Textual analysis of foundational Maliki works (Malik''s Muwatta, Ibn al-Qasim''s Mudawwana, al-Shatibi''s writings) documenting whether they present Maliki methodology as one legitimate approach among alternatives (implicit kernel contest) or as the superior/only correct approach (claim to exclusive authority)',
    'If kernel reading: the constraint models one position in an ongoing debate with other madhabs, and classification should emphasize the perspectival nature (tangled rope because contested). If exclusive claim: the constraint models an institutional assertion of singular authority, which would raise the extraction assessment and potentially shift classification toward snare for those bound by Maliki jurisdiction without choice of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_ambiguity, conceptual, 'Whether this is a reading within a contested kernel or a claim to exclusive methodological authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_theater_early_abbasid, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(maliki_theater_mid_abbasid, usul_al_fiqh_method__maliki_reading, theater_ratio, 250, 0.25).
narrative_ontology:measurement(maliki_theater_late_abbasid, usul_al_fiqh_method__maliki_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement(maliki_theater_ottoman, usul_al_fiqh_method__maliki_reading, theater_ratio, 800, 0.38).

% Extraction over time
narrative_ontology:measurement(maliki_extractiveness_early_abbasid, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(maliki_extractiveness_mid_abbasid, usul_al_fiqh_method__maliki_reading, base_extractiveness, 250, 0.38).
narrative_ontology:measurement(maliki_extractiveness_late_abbasid, usul_al_fiqh_method__maliki_reading, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(maliki_extractiveness_ottoman, usul_al_fiqh_method__maliki_reading, base_extractiveness, 800, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(maliki_suppression_early_abbasid, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maliki_suppression_late_abbasid, usul_al_fiqh_method__maliki_reading, suppression_requirement, 500, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul al-fiqh kernel produces four constraint stories, one per madhab reading. Each story has its own epsilon (ε) reflecting the degree of extraction inherent in that particular methodological framework. The Maliki reading (this constraint, ε=0.42) differs from Hanafi (ε=0.38, slightly less extractive due to broader juristic discretion reducing gatekeeping), Shafi'i (ε=0.35, lower extraction due to explicit hierarchical constraints), and Hanbali (ε=0.52, higher extraction due to gatekeeping by hadith specialists). The differences are not observable-dependent variations of a single constraint; they reflect structurally distinct methodologies with different beneficiaries, victims, and coordination functions. Each story is linked to its sibling readings via network.affects_constraints to enable cross-madhab analysis in the engine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, institutional, 0.35).
constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
