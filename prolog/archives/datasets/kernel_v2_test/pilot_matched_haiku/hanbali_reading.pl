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
 *   human_readable: Hanbali Jurisprudential Method: Text Literalism and Qiyas Restriction
 *   domain: islamic_jurisprudence/legal_theory
 *
 * SUMMARY:
 *   The Hanbali jurisprudential method represents one reading of the
 *   foundational usul al-fiqh (principles of jurisprudence) kernel — the
 *   stabilized commitment to scriptural authority and methodological rigor
 *   that all four major Islamic legal schools (Hanbali, Hanafi, Maliki,
 *   Shafi'i) interpret differently. The Hanbali reading emphasizes text
 *   literalism (strict adherence to Qur'anic and hadith language) and
 *   restricts qiyas (analogical reasoning), preferring weak hadith (da'if)
 *   over rational derivation when the two conflict. This constraint exhibits
 *   the full range of DR classification from different perspectives:
 *   traditionalist scholars experience it as coordination (Rope), rationalist
 *   jurists experience it as extraction (Snare), the hadith transmission
 *   authority experiences it as mixed coordination-extraction (Tangled Rope),
 *   and the Hanbali institutional structure maintains it theatrically despite
 *   partial functional atrophy (Piton). The constraint's extractiveness
 *   (0.35) reflects moderate asymmetry: traditionalists benefit from the
 *   method's authority concentration, while rationalists bear costs through
 *   restricted interpretive tools. The suppression (0.48) reflects
 *   institutional enforcement (doctrinal reaffirmation, scholarly
 *   gatekeeping) that has weakened over time as modern Hanbali jurisprudence
 *   increasingly employs qiyas in practice while maintaining theoretical
 *   prohibition. The theater ratio (0.38) reflects the gap between stated
 *   method (text literalism, qiyas restriction) and actual practice (modern
 *   Hanbali scholars use analogical reasoning while denying it).
 *
 * KEY AGENTS:
 *   - Traditionalist Scholars: Primary beneficiary (institutional/arbitrage) — text literalism and hadith preference concentrate interpretive authority in domains where traditionalists hold institutional dominance
 *   - Rationalist Jurists: Primary victim (powerless/identity_locked) — identity fused with qiyas practice; cannot exercise primary interpretive methodology without violating Hanbali framework
 *   - Hadith Transmission Authority: Secondary beneficiary (institutional/constrained) — weak-hadith preference elevates hadith scholarship's necessity and status; constrained by dependence on the constraint's persistence
 *   - Jurisprudential Innovation Movement: Secondary victim (organized/constrained) — benefits from textual stability but bears extraction costs through restricted innovation pathways
 *   - Hanbali School Institutional Structure: Institutional actor (institutional/arbitrage) — maintains the constraint theatrically; actual practice diverges from stated method
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as a necessary consequence of scriptural fidelity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.35).
domain_priors:suppression_score(hanbali_reading, 0.48).
domain_priors:theater_ratio(hanbali_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Jurisprudential Method: Text Literalism and Qiyas Restriction").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, '6aa96cc8-e289-4dba-9a96-2b0b6374ddf9').
narrative_ontology:cs_kernel_codification('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', fixed_text).
narrative_ontology:cs_authority_grounding('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', lineage).
narrative_ontology:cs_interpretation_layer_present('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9').
narrative_ontology:cs_reading_relation('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', hanbali_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', hanbali_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', hanbali_reading__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', foundational, text_literalism_necessity).
narrative_ontology:cs_axiom_status(text_literalism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', text_literalism_necessity, deontological).
narrative_ontology:cs_axiom('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', foundational, qiyas_restriction_principle).
narrative_ontology:cs_axiom_status(qiyas_restriction_principle, holdable).
narrative_ontology:cs_axiom_grounding('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', qiyas_restriction_principle, deontological).
narrative_ontology:cs_reference_frame('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', textual_primacy_with_hadith_authority).
narrative_ontology:cs_drift_state('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6aa96cc8-e289-4dba-9a96-2b0b6374ddf9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, traditionalist_scholars).
narrative_ontology:constraint_beneficiary(hanbali_reading, hadith_transmission_authority).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_interpreters).
narrative_ontology:constraint_victim(hanbali_reading, jurisprudential_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RATIONALIST JURIST (SNARE) — Identity fused with interpretive methodology (qiyas, analogical reasoning, rational derivation). Cannot exercise interpretive tools without violating the Hanbali framework's core prohibition. Structurally mobile (could adopt another school) but identity-locked: professional identity, scholarly reputation, and intellectual self-concept are constituted through rationalist jurisprudential practice. Maximum extraction experienced — the constraint forecloses the agent's primary interpretive modality.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: JURISPRUDENTIAL INNOVATION MOVEMENT (TANGLED ROPE) — Organized agents (reformist scholars, modernist interpreters) benefit from the Hanbali framework's textual rigor (it provides a stable foundation for reinterpretation) while bearing extraction costs (restricted innovation pathways, pressure to justify departures from text). Constrained exit: switching schools is possible but carries institutional and reputational cost. Mixed coordination (text-based stability) and extraction (method restriction).
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TRADITIONALIST SCHOLARLY ESTABLISHMENT (ROPE) — Primary beneficiary. The Hanbali method concentrates interpretive authority in hadith transmission and textual scholarship, domains where traditionalist scholars hold institutional dominance. Experiences the constraint as coordination: text literalism and weak-hadith preference stabilize the knowledge base and protect against unauthorized innovation. Net beneficiary with arbitrage options (can adopt alternative schools if needed, but institutional position is secure).
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HADITH TRANSMISSION AUTHORITY (TANGLED ROPE) — Institutional beneficiary. The Hanbali preference for weak hadith over qiyas elevates the status and necessity of hadith scholarship. Genuine coordination function: weak hadith requires careful chain-of-transmission analysis, which stabilizes the hadith corpus. But also extraction: the constraint restricts alternative verification methods (rational analysis, comparative jurisprudence) that might challenge hadith authority. Constrained exit: the authority's institutional position depends on the constraint's persistence.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HANBALI SCHOOL INSTITUTIONAL STRUCTURE (PITON) — The school's formal commitment to text literalism and qiyas restriction persists through institutional inertia and theological maintenance, but the functional justification has partially atrophied. Modern Hanbali jurisprudence often employs analogical reasoning (qiyas) in practice while maintaining the theoretical prohibition — the constraint is maintained theatrically through doctrinal reaffirmation rather than consistent application. Theater ratio reflects this gap between stated method and actual practice.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, text literalism appears as an immutable principle of scriptural interpretation: the Qur'an and Sunnah are fixed texts, and fidelity to them requires minimal interpretive mediation. This perspective naturalizes the Hanbali method as a logical consequence of textual primacy. However, the structural data reveals this as a false summit: the constraint benefits identifiable agents (traditionalist scholars, hadith authorities) and requires active enforcement against competing interpretive schools. The 'natural law' framing obscures the contingent institutional arrangements that sustain it.
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
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.35): Moderate. The Hanbali method benefits traditionalist scholars and hadith authorities through authority concentration, but the extraction is not severe because the method also provides genuine coordination benefits (textual stability, clear interpretive boundaries). The rationalist jurist experiences higher extraction (snare perspective), but the institutional beneficiaries experience lower extraction (rope perspective). The moderate value reflects the mixed coordination-extraction hybrid. Suppression (0.48): Moderate. Institutional enforcement through doctrinal reaffirmation and scholarly gatekeeping is real but has weakened over time. Modern Hanbali jurisprudence increasingly employs qiyas in practice, suggesting suppression is declining. The value reflects current enforcement intensity, not historical maximum. Theater ratio (0.38): Moderate. The gap between stated method (strict text literalism, qiyas restriction) and actual practice (modern Hanbali scholars use analogical reasoning) is substantial but not dominant. The constraint is maintained partly through genuine commitment to textual primacy and partly through institutional inertia. The rising trajectory (0.25 → 0.38) reflects increasing gap between theory and practice as modern jurisprudence evolves.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same methodological framework appears as coordination to beneficiaries and extraction to victims. The traditionalist scholar sees text literalism as a coordination mechanism that stabilizes the knowledge base and protects against unauthorized innovation (Rope). The rationalist jurist sees the same framework as a snare that forecloses their primary interpretive tool and locks them into a professional identity that depends on qiyas practice (Snare). The hadith transmission authority sees mixed coordination (weak hadith requires careful scholarship) and extraction (the constraint restricts alternative verification methods) (Tangled Rope). The Hanbali institutional structure sees its own method as increasingly performative — maintained through doctrinal reaffirmation rather than consistent application (Piton). The analytical observer risks naturalizing the constraint as a necessary consequence of scriptural fidelity (Mountain), but the structural data reveals this as a false summit: the constraint benefits identifiable agents and requires active enforcement against competing schools.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the constraint. Traditionalist scholars are beneficiaries with arbitrage options (can adopt other schools if needed) — low d, low effective extraction. Rationalist jurists are victims with identity-locked exit (cannot exercise their primary methodology without violating the framework) — high d, high effective extraction. The hadith transmission authority is a beneficiary with constrained exit (depends on the constraint's persistence) — moderate d, moderate effective extraction. The jurisprudential innovation movement is organized victims with constrained exit (can innovate within the framework but at higher cost) — moderate-high d, moderate effective extraction. The Hanbali institutional structure is a beneficiary with arbitrage options (can modify the constraint if needed) — low d, low effective extraction. The piton classification derives from the theater gate (high theater ratio indicates performative maintenance) rather than from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali reading resolves mandatrophy by showing that the constraint's mandate (preserve textual fidelity and prevent unauthorized innovation) has partially outlived its function. Modern Hanbali jurisprudence increasingly employs qiyas in practice while maintaining theoretical prohibition, suggesting the original mandate (prevent rationalist overreach) is no longer operative. The constraint persists through institutional inertia and doctrinal reaffirmation rather than functional necessity. The theater ratio's rising trajectory (0.25 → 0.38) confirms this: the gap between stated method and actual practice is widening, indicating the constraint is becoming increasingly performative. However, the constraint retains genuine coordination function (textual stability, clear interpretive boundaries), so it is not purely theatrical. The classification as Tangled Rope (not Piton) reflects that the coordination function remains real, even as the extraction mechanism persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_literalism_vs_contextual_reading,
    'Is text literalism a necessary consequence of scriptural fidelity, or a contingent methodological choice that other schools justify equally well through textual grounds?',
    'Comparative analysis of how Hanafi, Maliki, and Shafi''i schools justify their interpretive methods through Qur''anic and hadith evidence. If all schools cite the same textual sources to justify different methods, literalism is contingent, not necessary.',
    'If necessary: Hanbali reading is a mountain (natural law of scriptural interpretation). If contingent: Hanbali reading is a tangled rope (contingent institutional arrangement benefiting traditionalists). This is the core false-summit question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(text_literalism_vs_contextual_reading, conceptual, 'Whether text literalism is necessary or contingent').

omega_variable(
    weak_hadith_epistemic_status,
    'Does weak hadith (da''if) actually provide reliable guidance, or does the Hanbali preference for it over qiyas reflect institutional authority preservation rather than epistemic superiority?',
    'Historical analysis of jurisprudential outcomes: do weak-hadith-based rulings prove more durable and widely accepted than qiyas-based rulings across centuries? Comparison of error rates and reversals.',
    'If weak hadith proves epistemically superior: the constraint is justified coordination (Rope from more perspectives). If qiyas-based reasoning proves equally or more reliable: the constraint is extraction masquerading as methodology (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_epistemic_status, empirical, 'Epistemic reliability of weak hadith versus qiyas').

omega_variable(
    qiyas_restriction_as_reading_vs_kernel,
    'Is the qiyas restriction a reading of the Qur''an and Sunnah (interpretable differently by other schools), or a fixed kernel that all schools must accept?',
    'Textual analysis: do the Qur''an and Sunnah explicitly prohibit qiyas, or do they permit it? If they permit it, the Hanbali restriction is a reading (coexists_with other readings). If they prohibit it, the Hanbali reading forecloses others.',
    'If reading: Hanbali and other schools coexist as legitimate alternatives. If kernel: Hanbali reading forecloses rationalist schools'' core methodology. This determines the reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qiyas_restriction_as_reading_vs_kernel, conceptual, 'Whether qiyas restriction is a reading or a fixed kernel').

omega_variable(
    identity_lock_mechanism_in_rationalist_jurists,
    'Are rationalist jurists trapped by material barriers (institutional exclusion, career penalties) or identity-locked (their professional identity is constituted through qiyas practice)?',
    'Post-exit analysis: if a rationalist jurist adopts the Hanbali method, do they retain their scholarly identity and career trajectory, or does the shift require abandoning their intellectual self-concept? If the latter, the binding is identity-lock, not material trap.',
    'If trapped: the constraint is a snare with material suppression. If identity-locked: the constraint is a snare with cognitive suppression — the agent could exit structurally but cannot exit psychologically. Identity-lock suggests the constraint''s persistence depends on internalized framing, not external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_rationalist_jurists, empirical, 'Whether rationalist jurists are trapped or identity-locked').

omega_variable(
    hanbali_method_as_reading_of_usul_al_fiqh_kernel,
    'This constraint is one reading of the usul_al_fiqh_method kernel. What is the kernel itself, and how do the Hanbali, Hanafi, Maliki, and Shafi''i readings relate to it?',
    'Textual and historical analysis: identify the foundational Qur''anic and hadith sources all four schools cite as the basis for jurisprudential method. The kernel is the stabilized commitment (fixed text or practice-based norm) that all schools interpret differently.',
    'Clarifies whether the four readings coexist (each school interprets the kernel legitimately), or whether one reading forecloses others (one school''s interpretation is logically incompatible with another''s). This determines the network structure of the usul_al_fiqh_method constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanbali_method_as_reading_of_usul_al_fiqh_kernel, conceptual, 'The kernel and its readings in usul al-fiqh methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanb_tr_t0, hanbali_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanb_tr_t3, hanbali_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(hanb_tr_t6, hanbali_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(hanb_be_t0, hanbali_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hanb_be_t3, hanbali_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(hanb_be_t6, hanbali_reading, base_extractiveness, 6, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanb_su_t0, hanbali_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hanb_su_t3, hanbali_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(hanb_su_t6, hanbali_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one of four structurally distinct constraints in the usul_al_fiqh_method constraint family. Each reading has its own ε value, beneficiary/victim structure, and classification. The Hanbali reading emphasizes text literalism and qiyas restriction; the Hanafi reading permits greater qiyas; the Maliki reading emphasizes custom (urf) and public interest (maslaha); the Shafi'i reading balances text and reason. These are not the same constraint viewed from different angles — they are different constraints with different extractiveness values, different beneficiary/victim structures, and different institutional effects. The family is linked through network.affects_constraints: each reading influences the others by creating institutional pressure to justify departures from its method.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanbali_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
