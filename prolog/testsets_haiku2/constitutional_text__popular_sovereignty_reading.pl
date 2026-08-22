% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text as Popular Sovereignty Constraint
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of
 *   constitutional authority. Constitutional text is framed as deriving its
 *   binding force from the constituent power of the demos, not from courts or
 *   legislatures. Neither institutional actor is supreme; the people retain
 *   ultimate interpretive authority through formal amendment, constitutional
 *   convention, or revolutionary action. The constraint extracts from
 *   institutional actors (who lose authority claims) in service of democratic
 *   participation (who gain legitimacy claims), but extraction is modest
 *   (0.41) because the constraint generates benefits as well as costs: it
 *   coordinates legitimate claim-making and provides a channel for
 *   intergenerational constitutional contestation. Suppression is substantial
 *   (0.68) because institutional actors actively work to limit the practical
 *   scope of this reading, containing it to formal amendment procedures while
 *   resisting the idea that popular movements can override institutional
 *   interpretation through extra-institutional means. Theater is elevated
 *   (0.58) because much public discourse about popular sovereignty is
 *   performative—institutions invoke it rhetorically while constraining it
 *   institutionally, and movements invoke it while facing barriers to
 *   exercising it.
 *
 * KEY AGENTS:
 *   - Popular democratic movements and constituent assemblies (beneficiaries; claim interpretive authority)
 *   - Legislative bodies and court systems (payers; lose exclusive interpretive authority)
 *   - Institutional legal experts (payers; expertise is subordinated to popular will)
 *   - Ordinary citizens (theoretically beneficiary but identity-locked into powerlessness without collective mobilization)
 *   - Comparative constitutional scholars (observers; measure the reading across jurisdictions)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.41).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.68).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text as Popular Sovereignty Constraint").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '10d6089b-20f5-4298-948f-0ef5820b011e').
narrative_ontology:cs_kernel_codification('10d6089b-20f5-4298-948f-0ef5820b011e', fixed_text).
narrative_ontology:cs_authority_grounding('10d6089b-20f5-4298-948f-0ef5820b011e', extraction).
narrative_ontology:cs_interpretation_layer_present('10d6089b-20f5-4298-948f-0ef5820b011e').
narrative_ontology:cs_reading_relation('10d6089b-20f5-4298-948f-0ef5820b011e', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('10d6089b-20f5-4298-948f-0ef5820b011e', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('10d6089b-20f5-4298-948f-0ef5820b011e', foundational, constituent_power_ultimate_authority).
narrative_ontology:cs_axiom_status(constituent_power_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('10d6089b-20f5-4298-948f-0ef5820b011e', constituent_power_ultimate_authority, deontological).
narrative_ontology:cs_axiom('10d6089b-20f5-4298-948f-0ef5820b011e', secondary, institutional_subordination_to_demos).
narrative_ontology:cs_axiom_status(institutional_subordination_to_demos, holdable).
narrative_ontology:cs_axiom_grounding('10d6089b-20f5-4298-948f-0ef5820b011e', institutional_subordination_to_demos, deontological).
narrative_ontology:cs_reference_frame('10d6089b-20f5-4298-948f-0ef5820b011e', constituent_power_primacy).
narrative_ontology:cs_drift_state('10d6089b-20f5-4298-948f-0ef5820b011e', institutional_containment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10d6089b-20f5-4298-948f-0ef5820b011e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_democratic_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constituent_assemblies).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_legal_experts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_bodies).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, court_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, ordinary_citizens).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_primacy).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_amendment_authority).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, extra_institutional_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize periodically to assert that the people retain ultimate interpretive authority over the constitutional text. They invoke amendment procedures, constitutional conventions, or appeal to revolutionary right when institutional actors drift from what the demos considers legitimate constitutional meaning. They benefit from a reading that validates their claim to authorship and ongoing interpretive power.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_democratic_movements, beneficiary,
    organized, generational, mobile, national).

% Called during moments of constitutional rupture or renewal, they embody the demos' power to rewrite fundamental law. They benefit from the reading because it legitimizes their authority as superior to ordinary legislative or judicial interpretation. Their power is episodic but decisive when activated.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constituent_assemblies, beneficiary,
    powerful, generational, mobile, national).

% Operate under constitutional constraints they did not author and cannot unilaterally reinterpret. Under this reading they are subordinate to both courts (which interpret the text) and to the demos (which can amend it). They bear the cost of being bound by constitutional text they cannot override through ordinary legislation, and face the additional cost of legitimacy challenges when movements claim the demos' will diverges from legislative action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_bodies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, legislative_bodies, observer).

% Serve as interpreters of constitutional text, but under this reading they hold no supreme authority. Their interpretations can be overridden by amendment or by the demos' assertion of constituent power through extra-institutional means. They bear the cost of institutional vulnerability: their rulings stand only while the demos accepts them. Popular movements can delegitimize judicial authority by claiming the people's will diverges from judicial interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, court_systems, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, court_systems, observer).

% Claim professional authority to interpret constitutional text through doctrinal and historical methods. Under this reading, their expertise is subordinate to popular will; their interpretations can be overridden by movements claiming constituent power. They bear the cost of reduced epistemic authority: the demos is authorized to reject their professional judgment without needing to master their methodology.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_legal_experts, payer,
    powerful, biographical, constrained, national).

% Theoretically possess ultimate interpretive authority as members of the demos, but exercising it requires collective mobilization, constitutional amendment, or revolutionary action — all high-cost activities. They benefit from the reading's legitimizing narrative but face substantial barriers to exercising the power it claims they hold. Their identity as constituent members of the polity is what grounds their theoretical authority, making exit impossible except through expatriation.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, ordinary_citizens, beneficiary,
    powerless, biographical, identity_locked, national).

% Analyze how different constitutional systems instantiate or deny popular sovereignty. They observe the constraint's operation across jurisdictions, comparing amendment procedures, revolutionary moments, and institutional responses to popular movements. They neither collect from nor pay into the constraint; they measure its effects.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_jurists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the demos as the ultimate source of constitutional meaning, coordinating legitimate claim-making across time: constitutional text is binding not because courts say so or legislatures agree, but because the people remain the constituent authority. This coordinates appeals across generations and political movements to a common font of legitimacy. It also coordinates resistance to institutional drift by providing a framework for challenging institutions that claim to act within the constitution but violate the demos' understanding of it.
% TRANSFER_FUNCTION: Transfers authority from institutional actors (courts, legislatures) to extra-institutional mobilization (popular movements, constituent assemblies). It moves the burden of constitutional interpretation from expert judgment toward popular will, and places the cost of defending institutional positions on courts and legislatures who must now justify themselves to claimed popular sentiment, not vice versa.
% ABSENT_VOICES: Institutional actors who benefit from stability and expertise-based authority (career civil servants, tenured judges, constitutional scholars) are structurally excluded from this reading's legitimacy frame. They can speak, but their speech is heard as defending institutions against the people, not as the voice of legitimate authority. Legal formalists who believe constitutional meaning is text-determined and not subject to popular reinterpretation are excluded from the core claim. Economists and policy experts who argue constitutional change should move slowly to preserve institutional credibility are marginalized.
% DISAPPEARANCE_RATIONALE: If this reading vanished—if the demos lost all claim to ultimate constitutional authority and courts or legislatures became truly supreme—constitutional legitimacy would reground in institutional authority alone. The possibility of constitutional convention, popular amendment, and revolutionary change would lose their legitimating framing. Institutions would stabilize faster, but periodic movements claiming to speak for the people would lose a recognized channel for challenging constitutional interpretation. The coordinate system for intergenerational claims on constitutional meaning would collapse into purely institutional processes.
% FOUNDING_PROBLEM: Early modern constitutionalism faced the question: where does constitutional text derive its binding force? The popular sovereignty reading answers that it derives from the people's constituent power—the authority to establish fundamental law lies with the demos, not with any institution that claims to be supreme. This reading emerged as a response to both absolutist monarchies (which claimed institutions were supreme) and purely institutional readings (which cut the constitution's tie to popular will).
% FOUNDING_PROBLEM_CORROBORATION: Popular movements and constituent assemblies throughout the 18th–21st centuries have appealed to this reading (French Revolution's appeal to the nation, the call for the Philadelphia Convention, modern movements for constitutional reform). Comparative constitutional scholars document the pattern. Legal scholars advocating for popular constitutionalism (Ackerman, Tushnet) corroborate the reading from outside institutional beneficiary seats. However, judges and legislatures in many jurisdictions do not recognize popular sovereignty as operationally supreme; they treat it as a limiting principle but not a practical override to their own authority. The founding problem remains live and contested because institutions continue to claim interpretive finality while movements continue to claim the demos retains ultimate authority.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest because the constraint generates genuine coordination value (a framework for intergenerational constitutional claims) alongside its extraction (subordinating institutional authority to popular will). The measurement series shows extractiveness rising from 0.28 to 0.41 over the interval as movements increasingly invoke the reading to challenge institutional interpretations, gradually accumulating practical authority costs for institutions. Theater ratio declines from 0.72 to 0.58 as the reading becomes less purely rhetorical and more operationally invoked through actual amendment and constitutional convention activity (the index reflects the growing ratio of functional to performative operation). Suppression requirement rises from 0.54 to 0.68 as institutions must devote more enforcement resources to containing the reading—they invest in legal doctrines limiting popular sovereignty to formal amendment only, and in institutional credibility narratives designed to preempt movement challenges. Accessibility collapse at 0.62 reflects that ordinary citizens theoretically have access to constituent power but face substantial organizational and informational barriers; alternatives to institutional interpretation (amendment, convention, revolution) exist but are not cheap or easy.
 *
 * PERSPECTIVAL GAP:
 *   Institutional seats (courts, legislatures, legal experts) should compute this constraint differently from popular movement seats. From the institutional view, this reading represents a threat to the rule of law and expertise-based interpretation; from the movement view, it represents the foundation of democratic legitimacy. The engine computes per-seat classifications from the structural data: institutional payers will experience this as a snare-like constraint (extracted from without control), while organized democratic movements will experience it as rope-like (genuine coordination they benefit from). This divergence is structural and intentional—the same arrangement generates different effective extraction across seats with different power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Constituent assemblies and popular movements hold institutional or organized power with mobile exit options—they benefit from the reading (low directionality toward extraction). Ordinary citizens are theoretically beneficiaries but identity-locked (cannot exit the demos) and powerless—their directionality is complex: they have nominal authority but no mechanism to exercise it except through high-cost mobilization. Courts and legislatures are institutional actors whose power is constrained by this reading and who cannot exit institutional roles—they are targets of extraction (high directionality toward it). Legal experts are powerful but face subordination of their epistemic authority—their directionality is intermediate. The beneficiary/victim distinction maps to institutional versus extra-institutional authority: those who benefit from institutional certainty are victims; those who benefit from popular sovereignty claims are beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids misclassification as pure rope by explicitly naming victims (institutional authority, legal expertise) alongside beneficiaries (democratic participation). It avoids misclassification as snare by including genuine coordination function (intergenerational constitutional meaning-making). The tangled_rope classification holds because (1) it coordinates legitimate claim-making across time, (2) it extracts authority from institutions, (3) it requires active enforcement (institutions must constantly work to limit the reading's practical scope), and (4) asymmetry is clear: institutional actors are constrained while movements gain legitimacy. The theater ratio is elevated precisely because institutions invest in performing popular sovereignty (invoking it in decisions, holding referendums) while containing its practical scope—the performative/functional ratio reveals institutional ambivalence about the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_operationalization,
    'What mechanisms count as authentic expression of constituent power? Are formal amendment procedures the only legitimate expression, or can popular movements, constitutional conventions, and revolutionary claims all operate at the same level of authority?',
    'Historical analysis of actual constitutional moments: which mechanisms have successfully overridden institutional interpretation, and through what legitimacy frames? Comparative study of amendment success rates, convention outcomes, and movement victories across constitutional systems.',
    'If only formal amendment counts, the reading collapses into institutional procedure and loses its distinctive claim. If all mechanisms count equally, the reading validates revolutionary authority and generates deep institutional instability. The reading''s coherence depends on resolving this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constituent_power_operationalization, conceptual, 'Whether constituent power is limited to formal amendment or includes extra-institutional mechanisms.').

omega_variable(
    institutional_supremacy_coexistence,
    'Can institutional actors (courts or legislatures) genuinely function as supreme interpreters of constitutional meaning while also acknowledging that the demos retains ultimate authority? Or do these claims logically foreclose each other within a single framework?',
    'Analysis of working constitutional systems that nominally embrace popular sovereignty while operating with de facto institutional supremacy (e.g., systems with rare amendment and de facto judicial power). Do they function coherently, or do they generate periodic constitutional crises where the coexistence breaks down?',
    'If coexistence is logically stable, this reading coexists_with the institutional supremacy readings; if they logically foreclose each other, this reading forecloses the others. The nature of the kernel contest depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_supremacy_coexistence, conceptual, 'Whether popular sovereignty and institutional supremacy can coexist or logically foreclose each other.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of popular sovereignty expressions primarily structural (legal barriers, high transaction costs of amendment/convention) or internalized (citizens accept that constitutional interpretation belongs to experts, making mobilization psychologically difficult)?',
    'Post-crisis analysis: when barriers to popular mobilization are suddenly removed (e.g., during regime collapse, emergency situations), do suppressed mobilizations emerge rapidly? If so, suppression was more structural; if populations remain passive, suppression is more internalized.',
    'If primarily structural, removing barriers (lowering amendment thresholds, legitimizing constitutional conventions) could dramatically increase the constraint''s practical operation; if primarily internalized, legal change alone will not activate the reading. The mechanism matters for understanding whether this reading has dormant power or has been psychologically displaced by institutional authority narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of popular sovereignty claims is structural or internalized.').

omega_variable(
    reading_identity_kernelness,
    'Is ''popular sovereignty reading'' a genuine reading of a fixed kernel (the constitutional text), or is it a claim that the kernel itself—what the constitution IS—includes popular amendment power as part of its meaning? The distinction matters: one treats the reading as an interpretation of existing text; the other treats it as inscribing something INTO the text.',
    'Genealogical analysis: trace the reading''s invocation in actual constitutional moments. When movements or constituents invoke ''popular sovereignty,'' are they interpreting existing text, or are they claiming the text must include this principle? Do different historical moments frame it differently?',
    'If the reading is genuinely textual interpretation, the kernel is indeed fixed and three readings contest its meaning. If the reading inscribes a principle not present in the text, it is a different kind of claim and may not cleanly coexist with institutional supremacy readings. This affects whether all three readings are symmetric competitors or whether one is meta-level (asserting what must be true about the kernel for any interpretation to have legitimacy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_kernelness, conceptual, 'Whether the popular sovereignty reading interprets fixed constitutional text or inscribes a principle into it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__popular_sovereignty_reading, theater_ratio, 8, 0.68).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__popular_sovereignty_reading, theater_ratio, 16, 0.64).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__popular_sovereignty_reading, theater_ratio, 24, 0.61).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__popular_sovereignty_reading, theater_ratio, 32, 0.59).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text__popular_sovereignty_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text__popular_sovereignty_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text__popular_sovereignty_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text__popular_sovereignty_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text__popular_sovereignty_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text__popular_sovereignty_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text__popular_sovereignty_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text__popular_sovereignty_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The kernel 'constitutional_text' decomposes into three constraint stories, one per reading. Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. The popular_sovereignty_reading (this story) frames authority as distributed through popular mobilization; judicial_supremacy_reading concentrates authority in courts; legislative_sovereignty_reading concentrates it in parliaments. The three constraints form a family: each affects the others because movements claiming one reading delegitimize competing readings. They are not different measurements of one constraint—they are structurally distinct constraints under the same kernel. Network links: this story affects both siblings, and both siblings affect this one (bidirectional). Rationale: the authority-concentration axis is the ε-invariance fault line; crossing it produces different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.78).
constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
