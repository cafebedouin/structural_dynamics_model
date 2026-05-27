% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-07-17
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Reading: Text-Literalism and the Rejection of Analogical Reasoning in Islamic Jurisprudence
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   The Hanbali literalist reading of Islamic jurisprudence rejects
 *   analogical reasoning (qiyas), juristic preference (istihsan), and
 *   consideration of public interest (istislah) as illegitimate innovations
 *   (bid'ah) that corrupt the kernel. This reading grounds jurisprudential
 *   legitimacy exclusively in: (1) literal text of Qur'an and authenticated
 *   Hadith, (2) explicit opinions of the Prophet's Companions, and (3)
 *   unanimous consensus (ijma') when it can be established. The constraint
 *   exhibits high extractiveness because the reading delegitimizes entire
 *   classes of juristic reasoning that other schools (Hanafi, Maliki,
 *   Shafi'i) accept as necessary and valid. This creates an asymmetric
 *   institutional relationship: textualist scholars benefit from the
 *   constraint (it privileges their interpretive method), while rationalist
 *   jurists and customary-practice communities bear the extraction cost
 *   (their methods are declared invalid, their practices are deemed
 *   innovations). The extractiveness has increased over the measurement
 *   interval (0.32 → 0.58) as Hanbali institutional power has consolidated
 *   and the suppression of rival methods has intensified. Theater ratio
 *   (0.48) reflects that Hanbali jurisprudence requires both authentic
 *   textual grounding AND performative commitment to the claim of absolute
 *   literalism, when in fact even Hanbali scholars have always engaged in
 *   implicit interpretation (of hadith authentication, companion opinion
 *   precedence, and consensus boundaries). The constraint is a Tangled Rope
 *   because it genuinely solves a coordination problem (scriptural
 *   interpretation requires consistent methodology) while imposing asymmetric
 *   extraction (privileging one school's methodology and suppressing others).
 *
 * KEY AGENTS:
 *   - Textualist Scholars and Conservative Authorities (institutional/arbitrage): Primary beneficiary—the reading privileges literal-text interpretation, which is their interpretive comparative advantage; can exit by joining rival schools but don't because they benefit
 *   - Rationalist Jurists and Mujtahids (moderate/constrained): Primary victim—their juristic methods (qiyas, istihsan, istislah) are declared bid'ah; can exit by adopting rival jurisprudential schools but face reputation and institutional cost
 *   - Customary Practice Communities (powerless/trapped): Secondary victim—regional and temporal custom (urf) is delegitimized as innovation; cannot exit without abandoning cultural identity and social cohesion
 *   - Reformist and Modernist Movements (organized/constrained): Secondary victim/beneficiary—literalism enables salafi reform but denies juristic flexibility needed for novel contexts
 *   - Traditional Hanbali Institutional Hierarchy (institutional/arbitrage): Institutional actor maintaining the reading through transmission; now experiencing piton degradation (theater maintenance exceeds functional justification)
 *   - Comparative Jurisprudence / Analytical Observer (analytical/analytical): Sees the reading as one coherent solution among four legitimate Sunni jurisprudential methods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.58).
domain_priors:suppression_score(hanbali_reading, 0.65).
domain_priors:theater_ratio(hanbali_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Reading: Text-Literalism and the Rejection of Analogical Reasoning in Islamic Jurisprudence").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, '35fb1aeb-7467-4779-9099-feadeab36b01').
narrative_ontology:cs_created_at('35fb1aeb-7467-4779-9099-feadeab36b01', '').
narrative_ontology:cs_kernel_codification('35fb1aeb-7467-4779-9099-feadeab36b01', fixed_text).
narrative_ontology:cs_authority_grounding('35fb1aeb-7467-4779-9099-feadeab36b01', lineage).
narrative_ontology:cs_interpretation_layer_present('35fb1aeb-7467-4779-9099-feadeab36b01').
narrative_ontology:cs_kernel_id(hanbali_reading, jurisprudential_method_kernel).
narrative_ontology:cs_reading_relation('35fb1aeb-7467-4779-9099-feadeab36b01', hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('35fb1aeb-7467-4779-9099-feadeab36b01', maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('35fb1aeb-7467-4779-9099-feadeab36b01', shafii_reading, coexists_with).
narrative_ontology:cs_axiom('35fb1aeb-7467-4779-9099-feadeab36b01', foundational, qiyas_is_bid_ah).
narrative_ontology:cs_axiom_status(qiyas_is_bid_ah, holdable).
narrative_ontology:cs_axiom_grounding('35fb1aeb-7467-4779-9099-feadeab36b01', qiyas_is_bid_ah, empirically_contingent).
narrative_ontology:cs_axiom('35fb1aeb-7467-4779-9099-feadeab36b01', foundational, textual_sufficiency).
narrative_ontology:cs_axiom_status(textual_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('35fb1aeb-7467-4779-9099-feadeab36b01', textual_sufficiency, deontological).
narrative_ontology:cs_reference_frame('35fb1aeb-7467-4779-9099-feadeab36b01', scriptural_literalism).
narrative_ontology:cs_drift_state('35fb1aeb-7467-4779-9099-feadeab36b01', contemporary_institutional_power, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, conservative_authorities).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(hanbali_reading, adaptive_legal_reasoning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUSTOMARY PRACTICE COMMUNITIES (SNARE) — Bound by regional and temporal custom (urf) that diverges from literal textual interpretation. The Hanbali reading denies the validity of customary practice as juristic source. Communities face extraction: their lived practices are delegitimized as bid'ah, yet they cannot exit without abandoning cultural identity and social cohesion. Maximum suppression — no exit option except wholesale cultural transformation.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RATIONALIST JURISTS AND MUJTAHIDS (TANGLED ROPE) — Experience both coordination benefit (the Hanbali reading provides a stable textual foundation) and extraction (their juristic methods—qiyas (analogical reasoning), istihsan (juristic preference), istislah (public interest)—are declared invalid innovations). Constrained: they can adopt alternative jurisprudential schools, but doing so costs institutional standing and scholarly reputation within Hanbali-dominated contexts. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEXTUALIST SCHOLARS AND CONSERVATIVE AUTHORITIES (ROPE) — Primary beneficiaries. The reading secures their institutional authority by privileging the interpretive method they control (literal text, hadith authentication, companion opinions). Benefits from coordination: enables consistent jurisprudential outcomes within Hanbali framework. Can exit by adopting rival schools (arbitrage option), but do not because the constraint benefits them. Net beneficiary—experienced as pure coordination.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORMIST AND MODERNIST MOVEMENTS (TANGLED ROPE) — Organized agents (18th–20th century mujaddidun, contemporary Islamic-reform scholars) view the Hanbali literalist constraint as both enabling and restrictive. Enabling: return-to-sources (salafi) methodology provides epistemic grounding for institutional reform. Restrictive: literalism denies the juristic flexibility needed to address novel contexts (modern finance, internet regulation, gender equality). Constrained: reformers must work within Qur'anic/Hadith textual language or risk illegitimacy. Moderate extraction with asymmetric benefit distribution.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL HANBALI INSTITUTIONAL HIERARCHY (PITON) — The literalist methodology now persists partly through institutional inertia. Early Hanbali theorists (Ahmad ibn Hanbal, Ibn Qayyim al-Jawziyya) articulated text-literalism as a coherent jurisprudential stance; contemporary Hanbali institutions maintain the stance through transmission and authority, but with degraded functional justification. Modern Hanbali scholars acknowledge that some degree of juristic reasoning is unavoidable (even recognizing madhab variants and ikhtilaf—legitimate disagreement—represents a retreat from pure literalism). Theater ratio (0.48) reflects that institutional maintenance of the stance requires both authentic textual grounding AND performative ritual of claiming absolute fidelity to it.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPARATIVE JURISPRUDENCE / ANALYTICAL OBSERVER (ROPE) — From the civilizational/universal perspective, the Hanbali reading is one coherent solution to the coordination problem of deriving consistent law from scriptural sources. All four Sunni schools (Hanafi, Maliki, Shafi'i, Hanbali) solve this problem differently; the analytical observer sees the Hanbali method as coordination, not extraction. This perspective risks naturalizing the constraint as inevitable—a logical entailment of monotheistic jurisprudence—but the structural data reveals it as one chosen institutional arrangement among legitimate alternatives.
constraint_indexing:constraint_classification(hanbali_reading, rope,
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

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Hanbali reading extracts significant cost from rationalist jurists by denying legitimacy to their methods; it extracts cost from customary-practice communities by delegitimizing their lived legal practices. However, the extraction is not total (χ per perspectives varies widely, from Rope for beneficiaries to Snare for powerless), and the constraint does serve genuine coordination function (consistent jurisprudential methodology). The trajectory shows accumulation: early Hanbali jurisprudence (0.32) was pluralistic in practice; as institutional power consolidated, the literalist claim hardened and extraction increased (0.58). Suppression (0.65): Moderate-high. Significant barriers to adopting rival jurisprudential methods include institutional authority (Hanbali scholars control religious institutions in certain regions), social legitimacy (association with Salafi reform gives literalism prestige), and cultural identity (adopting rival schools implies adoption of rival cultural-legal frameworks). Customary-practice communities face suppression both structural (legal authority denies their practices) and internalized (cultural identity fused with literalist interpretation). Theater ratio (0.48): Moderate. The Hanbali reading has genuine textual grounding—the Qur'an and Hadith do privilege literal language—but it also requires performative commitment to the claim of absolute literalism. In practice, even Hanbali scholars covertly apply interpretation: hadith authentication involves judgment about which sources are reliable; selecting among competing Companion opinions involves implicit reasoning about precedence; determining consensus boundaries requires interpretation of what constitutes 'agreement.' The theater-ratio trajectory shows increasing performativity as the constraint matured: early Hanbali scholars (0.35) were candid about interpretive activity; later scholars (0.48) maintain stricter literalist posture despite equivalent practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Textualist scholars experience the constraint as pure coordination (Rope)—they are solving the legitimate problem of consistent scriptural interpretation and are not oppressed by the outcome. Rationalist jurists experience it as mixed coordination and extraction (Tangled Rope)—the methodology is coherent but disadvantages their approach. Customary-practice communities experience it as pure extraction (Snare)—their practices are delegitimized with no exit option. Reformist movements experience it as mixed (Tangled Rope with asymmetric benefit)—literalism enables their theological reform but constrains their juristic flexibility. The Hanbali institutional hierarchy experiences it as degraded (Piton)—the method persists through inertia, not functional justification. The analytical observer risks seeing it as inevitable natural law (Rope for all), when in fact the reading is a contingent institutional choice among legitimate alternatives. The perspectival gap reveals that the constraint is not a Natural Law of scriptural interpretation but a political-institutional arrangement that benefits textualists at the expense of rationalists and communities rooted in customary practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and exit options. Textualist scholars are beneficiaries with arbitrage options (can join rival schools)—low d (≈0.15), yielding low χ. Rationalist jurists are victims with constrained options (high cost but possible exit)—moderate d (≈0.65), yielding moderate χ. Customary-practice communities are victims with trapped options (no exit without identity dissolution)—high d (≈0.85), yielding high χ. Reformist movements are both (benefit from literalism's theological stance, harmed by juristic inflexibility)—moderate d (≈0.50). The piton perspective uses canonical d for institutional power (≈0.00), yielding institutional baseline χ. The analytical perspective uses canonical d for analytical power (≈0.73), yielding moderate χ with civilizational scope. The perspectival gap in d-values reflects real structural differentiation: different agents occupy genuinely different positions relative to the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING ANALYSIS: The Hanbali reading instantiates one pole of the jurisprudential-method kernel. The mandatrophy question is: 'Is scriptural law derived through literal text + companion opinion + consensus (Hanbali), or through literal text + analogical reasoning + juristic preference + public interest (Hanafi/Maliki/Shafi'i)?' The Hanbali reading resolves mandatrophy by declaring that all valid jurisprudential reasoning must be textually grounded, and any method that goes beyond text (qiyas, istihsan, istislah) is bid'ah. This creates the tangled-rope structure: the reading offers genuine coordination (consistent methodology) while denying legitimacy to rival methodologies, which is extraction against rationalist jurists. The false-summit risk is that the analytical observer sees the literalist reading as 'faithful to scripture itself' (natural law) when in fact it is a contingent institutional choice—scripture permits multiple interpretive methods, and the reading's privileging of literalism is an enforcement choice, not a logical entailment. The measurement trajectory shows that extractiveness accumulates as the reading becomes institutionally dominant; early pluralism (0.32) gave way to stricter enforcement (0.58). Theater ratio shows that as the constraint matured, the institutional maintenance required more performative commitment because the functional grounding became weaker—even Hanbali scholars could not sustain pure literalism and implicitly applied interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_necessity_threshold,
    'At what empirical complexity threshold does literal textual interpretation require supplementation by analogical reasoning (qiyas) to remain functional?',
    'Historical documentation: comparison of Hanbali rulings on novel matters (inheritance of adopted children, profit-sharing contracts, non-Muslim minorities'' legal status) with rulings under Hanafi/Maliki schools; analysis of whether Hanbali schools covertly applied qiyas despite denying it',
    'If threshold is low: literalism is a structural fiction and rationalist jurists are right that qiyas is necessary. If threshold is high: literalism can sustain governance within bounded contexts, supporting textualist claim to closure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qiyas_necessity_threshold, empirical, 'Complexity threshold at which literalism requires analogical reasoning').

omega_variable(
    interpretation_layer_circularity,
    'Does the Hanbali emphasis on ''correct hadith authentication'' constitute an interpretive layer that permits drift equivalent to the rational schools'' juristic methods?',
    'Detailed philological analysis of hadith authentication methodology (isnad criticism, matn evaluation); comparison with juristic reasoning chains in rival schools; documentation of instances where Hanbali scholars chose among competing hadith sources or authenticated variant readings',
    'If authentication is an interpretive layer: the Hanbali reading is not truly literalist but has displaced interpretive reasoning into philology, claiming literalism while practicing rational method. If authentication is mechanical: literalism is structurally coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_layer_circularity, empirical, 'Whether hadith authentication is an interpretive layer equivalent to qiyas').

omega_variable(
    companion_consensus_availability,
    'How many novel juridical questions can be resolved by appeal to Companion opinions alone, without recourse to hadith interpretation or juristic reasoning?',
    'Quantitative analysis of Qur''anic rulings + Hadith rulings + Companion opinions on core juristic domains (contracts, family law, criminal law, ritual law); measurement of coverage gaps; historical documentation of how Hanbali scholars filled gaps',
    'If coverage is comprehensive: literal text + companion opinions can sustain closed system; Hanbali claim is empirically defensible. If coverage is sparse: Hanbali jurisprudence depends on implicit interpretation and reasoning to achieve completeness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companion_consensus_availability, empirical, 'Scope of juridical questions resolvable by literal text and Companion opinions').

omega_variable(
    bid_ah_boundary_incoherence,
    'What counts as bid''ah (blameworthy innovation) versus ijtihad (legitimate juristic reasoning), and can the boundary be defined without recourse to rationalist categories?',
    'Textual examination of Qur''an and Hadith to establish whether ''bid''ah'' is defined explicitly or requires interpretive elaboration; analysis of historical Hanbali treatises on bid''ah classification; comparison with how Hanafi, Maliki, Shafi''i schools draw the same boundary',
    'If boundary is textually explicit: Hanbali literalism is coherent. If boundary requires interpretive elaboration: literalism masks rational reasoning and the bin''ah/ijtihad distinction is unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_ah_boundary_incoherence, conceptual, 'Coherence of the bid''ah boundary without rationalist categories').

omega_variable(
    theological_vs_juristic_literalism,
    'Is the Hanbali literalist methodology primarily a theological commitment (rejecting rationalist theology—Mu''tazilism) or a juristic methodology, or both? Can these be disentangled?',
    'Historical analysis of Ibn Hanbal''s life and teachings; examination of whether early Hanbalites adopted literalism in response to theological threats (Mu''tazilite rational theology) or as an independent juristic principle; documentation of whether post-theological-triumph Hanbalites retained literalism for its own sake or as institutional inertia',
    'If primarily theological: literalism is a response to a specific historical threat; its persistence today is institutional inertia (piton), not logical necessity. If primarily juristic: literalism is self-contained and can persist independent of theological context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_juristic_literalism, conceptual, 'Whether literalism is theological reaction or independent juristic principle').

omega_variable(
    customary_practice_suppression_internalization,
    'Is the suppression of customary practice (urf) experienced by regional communities as external constraint (material barriers to alternative practice), internalized constraint (cultural identity fused with Hanbali literalism), or both?',
    'Qualitative research with Hanbali-dominated communities; documentation of whether communities that abandon Hanbali literalism retain customary practices or if the internalization persists; cross-cultural comparison with communities where literalism is imposed vs. organically adopted',
    'If external: suppression is structural; exit would be possible if barriers fell. If internalized: suppression is epistemic; exit requires identity transformation and the constraint persists even after institutional barriers remove.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_practice_suppression_internalization, empirical, 'Whether customary-practice suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanb_tr_t0, hanbali_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hanb_tr_t300, hanbali_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(hanb_tr_t600, hanbali_reading, theater_ratio, 600, 0.48).

% Extraction over time
narrative_ontology:measurement(hanb_be_t0, hanbali_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hanb_be_t300, hanbali_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(hanb_be_t600, hanbali_reading, base_extractiveness, 600, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential-method kernel decomposes into four constraint stories, one per Sunni school. Each story has a different ε (Hanbali has the highest because it delegitimizes rival methods; Hanafi has moderate ε because it integrates rivals' insights into a synthetic framework). The four stories are linked via network.affects_constraints: Hanbali influences the others because textualist prestige (modern Salafi reform) gives Hanbali literalism institutional power that creates pressure on rival schools to justify their rational methods. The constraints are not alternatives (only one kernel reading is true); they are simultaneous instantiations of a contested kernel by different juristic communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
