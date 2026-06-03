% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalism: Adaptive Interpretation of Evolving Meaning
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   Living constitutionalism — the interpretive reading that constitutional
 *   meaning evolves with society and judges must adapt principles to
 *   contemporary circumstances — is ONE reading of the contested US
 *   Constitution kernel. This reading operates within the formal authority
 *   structure of American constitutional law (Article III judicial power to
 *   interpret the Constitution) and competes with originalism (meaning fixed
 *   at ratification) and legal positivism (validity derives from formal
 *   enactment, not meaning content). The living constitutionalist reading
 *   empowers judges to treat post-ratification social change and evolving
 *   constitutional interpretation as authoritative sources for meaning,
 *   enabling new rights claims (reproductive freedom, marriage equality,
 *   privacy) that the Framers' original meaning explicitly excluded. This
 *   reading distributes beneficiary status to rights claimants in changed
 *   social contexts and victim status to claims about fixed constitutional
 *   meaning as a democratic constraint on judicial power. The constraint
 *   exhibits tangled rope structure from the institutional judicial
 *   perspective (coordination function enabling governance + extraction
 *   opportunity via discretionary reinterpretation), rope structure from
 *   rights claimants (coordination: enabling their participation), snare
 *   structure from fixed-meaning advocates (extraction of interpretive
 *   authority), and false-summit mountain structure from the analytical
 *   perspective (naturalization of contingent institutional choice).
 *
 * KEY AGENTS:
 *   - Rights Claimants in Changed Contexts: Beneficiary (moderate/constrained) — individuals seeking recognition of rights absent from original meaning (reproductive freedom, marriage equality, gender identity); experience the constraint as enabling their constitutional participation
 *   - The Judiciary (Institutional Judges): Beneficiary and agent of coordination (institutional/arbitrage) — empowered to interpret and adapt constitutional meaning; captures discretionary authority and career advancement through high-profile constitutional decisions; experiences both coordination function (resolving novel disputes) and extraction opportunity (power to reshape meaning)
 *   - Fixed Meaning Advocates: Victim (powerless/trapped) — constitutional scholars, originalist judges, rule-of-law defenders arguing that determinate meaning at ratification should constrain interpretation; experience living constitutionalism as suppression of the fixed-meaning constraint and extraction of democratic amendment authority
 *   - Amendment Institutions (legislatures, political coalitions): Organized agent (organized/constrained) — actors who could pursue formal constitutional amendment; see living constitutionalism as a temporary institutional arrangement that will be superseded by amendment when sufficient democratic will forms
 *   - Legal Positivism (Rule-of-law formalism): Institutional artifact (institutional/arbitrage) — the formal rule that constitutional interpretation should defer to text and original meaning persists as a ritualized expectation; honored ceremonially while bypassed functionally, operating as degraded institutional theater
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks treating the institutional choice to delegate adaptation authority to courts as a structural necessity inherent to any constitutional system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.28).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalism: Adaptive Interpretation of Evolving Meaning").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '7c42a119-7556-4b0e-a7cf-64a065a0a18a').
narrative_ontology:cs_kernel_codification('7c42a119-7556-4b0e-a7cf-64a065a0a18a', fixed_text).
narrative_ontology:cs_authority_grounding('7c42a119-7556-4b0e-a7cf-64a065a0a18a', lineage).
narrative_ontology:cs_interpretation_layer_present('7c42a119-7556-4b0e-a7cf-64a065a0a18a').
narrative_ontology:cs_reading_relation('7c42a119-7556-4b0e-a7cf-64a065a0a18a', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c42a119-7556-4b0e-a7cf-64a065a0a18a', us_constitution_text__positivist_reading, influences).
narrative_ontology:cs_axiom('7c42a119-7556-4b0e-a7cf-64a065a0a18a', foundational, meaning_necessarily_evolves).
narrative_ontology:cs_axiom_status(meaning_necessarily_evolves, holdable).
narrative_ontology:cs_axiom_grounding('7c42a119-7556-4b0e-a7cf-64a065a0a18a', meaning_necessarily_evolves, empirically_contingent).
narrative_ontology:cs_axiom('7c42a119-7556-4b0e-a7cf-64a065a0a18a', foundational, judges_delegate_adapters).
narrative_ontology:cs_axiom_status(judges_delegate_adapters, holdable).
narrative_ontology:cs_axiom_grounding('7c42a119-7556-4b0e-a7cf-64a065a0a18a', judges_delegate_adapters, conventional).
narrative_ontology:cs_reference_frame('7c42a119-7556-4b0e-a7cf-64a065a0a18a', adaptive_jurisprudence_framework).
narrative_ontology:cs_drift_state('7c42a119-7556-4b0e-a7cf-64a065a0a18a', contemporary_post_amendment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c42a119-7556-4b0e-a7cf-64a065a0a18a', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_contemporary_contexts).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS CLAIMANTS IN CHANGED SOCIAL CONTEXTS (ROPE) — Individuals seeking recognition of rights (reproductive freedom, marriage equality, digital privacy) under principles whose original meaning excluded them. Living constitutionalism treats their claims as coordination problems: adapting constitutional meaning to new social realities enables new constituencies to participate in the constitutional order. Exit options are constrained (cannot simply exit the constitutional system), but the constraint operates primarily as coordination rather than extraction. The beneficiary experiences this as enabling their participation.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE JUDICIARY AS ADAPTIVE INTERPRETERS (TANGLED ROPE) — Federal and state courts empowered to interpret constitutional principles in light of contemporary social understanding. This perspective experiences both coordination benefit (resolving novel disputes in a living constitutional order) and extraction opportunity (judicial discretion to reshape meaning; career advancement through high-profile constitutional decisions). Exit is available via arbitrage: judges can move between interpretive methodologies or decline to decide novel constitutional questions. The institutional beneficiary experiences real coordination function alongside institutional power accumulation.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CLAIMS TO FIXED CONSTITUTIONAL MEANING AS DEMOCRATIC CONSTRAINT (SNARE) — The principle of fixed meaning at ratification — treated as a check on judicial discretion — is suppressed by living constitutionalism's framework. This perspective views the constraint as extractive: judges capture interpretive authority by claiming the Constitution evolves with society, evading the democratic constraint that meaning should be fixed and revisable only through amendment. The victim here is not a person or group but the structural claim itself: that constitutional meaning has a determinate referent that could check judicial power. Trapped, powerless, with no exit from judicial reinterpretation.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL POWER CENTERS (EXECUTIVE/LEGISLATIVE) (TANGLED ROPE) — Branches of government that benefit from judicial adaptation of constitutional meaning when it aligns with their policy goals, but experience constraint when courts interpret against them. Exit is mobile: these branches can propose constitutional amendments or re-contest interpretation through litigation strategy. The constraint mixes coordination (courts resolve ambiguities that executive/legislative branches cannot) with extraction (courts unilaterally reshape constitutional obligations). Experience depends on whether judicial adaptation reinforces or constrains institutional power.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMENDMENT MOVEMENT (SCAFFOLD) — Organized political actors (legislatures, amendment coalitions, democratic renewal movements) see living constitutionalism as a temporary institutional arrangement that will eventually be superseded by formal amendment. The constraint enables interim governance while amendment mechanisms are activated. Low extraction because amendment movements see a clear sunset: once sufficient democratic will forms to amend (whether to constitutionalize new rights or restore fixed meaning), the living interpretation framework loses force. Constrained exit via the formal amendment Article V process.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL POSITIVISM AS INSTITUTIONAL ARTIFACT (PITON) — The formal rule-of-law principle that constitutional interpretation should defer to text and original meaning operates as a ritualized expectation that living constitutionalism simultaneously honors and bypasses. Courts invoke originalism as constraint on their own power while practicing living interpretation. The positivist rule persists through institutional theater: judges cite original meaning, discuss fidelity to text, perform constraint even as they adapt meaning. Theater ratio high because the formal apparatus (text, original meaning, rule of law) remains central to legitimacy narratives even as its functional constraint has degraded. Piton classification reflects degraded gatekeeping function maintained through ritual compliance.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, constitutional meaning must evolve with society — this is not a contingent choice but an inherent feature of how human communities maintain legal orders across generations and social change. Any fixed-meaning constitutional system would either become irrelevant to social reality or require continuous formal amendment (impossible for complex societies). Living constitutionalism appears not as judicial strategy but as a structural necessity: constitutional meaning necessarily changes or the document becomes a historical artifact, not a governing law. However, the institutional data reveals beneficiary groups and measurable extraction, contradicting the natural law reading.
constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_text__living_constitutionalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The living constitutionalist reading empowers judicial adaptation and captures interpretive discretion, but the extraction is mixed with genuine coordination function — courts do enable governance in changed circumstances, provide venues for novel rights claims, and resolve disputes that amendment could not rapidly address. The extractiveness reflects the asymmetric distribution of authority (judges, not citizens, decide meaning) and the beneficiary asymmetry (rights claimants benefit from judicial adaptation, while fixed-meaning advocates experience authority loss). The value increased over the interval (0.18 to 0.38) because judicial discretion expanded as social change accelerated and the gap between original meaning and contemporary practice widened. Suppression (0.28): Moderate-low. This reading explicitly reduces suppression of adaptive interpretation — living constitutionalism removes obstacles to judges adapting meaning. However, suppression operates at a different level: fixed-meaning claims are suppressed (denied legitimacy), and citizens' amendment authority is suppressed (bypassed through judicial reinterpretation rather than formal amendment). The value is lower than snare-range suppression because the framework does not entirely foreclose fixed-meaning arguments (they persist in originalist jurisprudence) or amendment (formal amendment remains technically available, though expensive). Theater ratio (0.52): Moderate. Living constitutionalism maintains significant performative elements: judges cite original meaning and text fidelity while practicing adaptive interpretation, invoke rule-of-law language while exercising discretion, invoke precedent and constraint while reshaping doctrine. The theater ratios are not as high as pure piton (theater < 0.70 threshold) because some genuine functional coordination exists alongside the performance.
 *
 * PERSPECTIVAL GAP:
 *   Living constitutionalism produces maximum perspectival divergence. Rights claimants see rope (coordination enabling their participation). Judges see tangled rope (coordination + extraction opportunity). Fixed-meaning advocates see snare (extraction of their constraint). Amendment institutions see scaffold (temporary institution with sunset via amendment). Legal positivism sees piton (performative constraint ritually maintained but functionally degraded). The analytical observer risks seeing mountain (structural necessity) when the institutional data reveals contingent choice. The perspectival gap reveals the constraint's dual structure: coordination function (enabling governance and rights recognition in changed circumstances) interwoven with extraction mechanism (judicial discretion to reshape meaning and displace amendment authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Living constitutionalism operates as a constraint grounded in judicial authority to interpret the Constitution. The directionality analysis maps agents' structural positions: Rights claimants (beneficiaries) see the constraint as enabling their participation — low d (beneficiary with constrained exit from the system). Judges (institutional beneficiaries) experience arbitrage exit — low-to-moderate d (benefit from interpretive discretion but face formal constraints via precedent and constitutional text). Fixed-meaning advocates (victims) experience trapped exit — high d (their structural claim about determinate meaning is suppressed and they cannot exit the judicially interpreted system). Amendment institutions (organized victims of functional foreclosure) experience constrained exit — moderate-high d (formal amendment remains available but expensive, making living interpretation more efficient). The analytical observer's perspective derives d from observation position (0.72 canonical d for analytical) and risks false-summit classification by naturalizing what are institutional choices about judicial delegation.
 *
 * MANDATROPHY ANALYSIS:
 *   Living constitutionalism resolves mandatrophy by demonstrating that it is genuinely a tangled rope — it contains authentic coordination function (resolving novel disputes, enabling participation of new constituencies, adapting principles to social change) alongside asymmetric extraction (judges capture interpretive authority that could belong to amendment institutions or remain with original meaning). The constraint is not pure extraction (snare) because coordination benefits are real; it is not pure coordination (rope) because beneficiary asymmetry is substantial. The mandatrophy resolution: accept the mixed structure as legitimate within constitutional design that delegates adaptive authority to courts precisely because amendment is expensive and slow. The cost is judicial discretion; the benefit is living governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaning_determinacy_ontology,
    'Does constitutional meaning have a determinate referent (fixed at ratification or in the text) that could in principle constrain interpretation? Or is meaning necessarily indeterminate until judges decide?',
    'Historiographical analysis of Framers'' intent recovery projects; comparison of originalist reconstructions across interpreters; correlation between textual ambiguity and divergent interpretive outcomes across cases',
    'If meaning is determinate: living constitutionalism represents judicial discretion masquerading as interpretation (extraction). If meaning is necessarily indeterminate: all interpretation is adaptive by structural necessity (coordination). The constraint''s classification hinges on this ontological claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meaning_determinacy_ontology, conceptual, 'Whether constitutional meaning has determinate referent independent of interpretation').

omega_variable(
    judicial_legitimacy_source,
    'Is judicial authority to interpret the Constitution grounded in fidelity to original meaning, in democratic responsiveness to contemporary values, or in formal authority to decide cases?',
    'Analysis of judicial opinions: frequency of originalist vs living constitutionalist rhetoric; correlation between stated methodology and actual outcomes; examination of cases where methodology reversed outcome',
    'If grounded in original meaning: judges are constrained interpreters (lower extraction). If grounded in contemporary values: judges are delegated adapters (higher extraction). If grounded in formal authority: methodology is theater (piton structure confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_source, conceptual, 'Source of judicial authority and interpretive legitimacy').

omega_variable(
    amendment_foreclosure_mechanism,
    'Does living constitutionalism functionally foreclose or delay formal constitutional amendment by providing an alternative mechanism for updating?',
    'Comparative analysis: societies with and without judicial constitutional adaptation; historical record of amendment attempts deferred when courts adapted meaning; cost-benefit analysis of amendment vs reinterpretation for different stakeholder groups',
    'If foreclosure: living constitutionalism extracts democratic constraint (Snare). If delay without foreclosure: amendment remains available (Tangled Rope). If acceleration: reinterpretation creates political pressure for formal amendment (Scaffold with sunset logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_foreclosure_mechanism, empirical, 'Whether judicial adaptation forecloses, delays, or accelerates formal amendment').

omega_variable(
    reading_foreclosure_question,
    'Does living constitutionalism''s core premise logically foreclose originalism within any single legal framework, or do both readings coexist as live positions in contemporary jurisprudence?',
    'Doctrinal analysis: can originalism and living constitutionalism both be held within Supreme Court''s reasoning in a single opinion? Do courts oscillate between methodologies for different clauses or across time? Can a jurist be consistently originalist while acknowledging constitutional evolution?',
    'If foreclosure holds: no coherent framework can contain both (forecloses relation). If coexistence: different justices hold different readings simultaneously (coexists_with relation). If influence: one reading constrains the other''s range (influences relation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Logical relationship between living constitutionalist and originalist readings').

omega_variable(
    false_summit_natural_law,
    'Is the natural law reading (living constitutionalism as structural necessity) a genuine natural law, or does it naturalize what are actually contingent institutional choices about delegation and amendment mechanics?',
    'Comparative constitutional law: examination of non-living constitutional systems (those with fixed meaning, formal amendment-only revision); historical analysis of periods when living constitutionalism was NOT the operative mode; theoretical assessment of whether evolution is inherent to any written constitution or specific to delegation structures',
    'If structural necessity: mountain classification is correct. If institutional choice: false summit signature fires, reclassifying to tangled_rope or snare depending on extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether constitutional evolution is structurally necessary or contingently chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_const_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(living_const_tr_t50, us_constitution_text__living_constitutionalist_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(living_const_tr_t100, us_constitution_text__living_constitutionalist_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(living_const_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(living_const_be_t50, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(living_const_be_t100, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, judicial_discretion_vs_rule_of_law).

% DUAL FORMULATION NOTE:
% The US Constitution kernel has three structurally distinct readings: living constitutionalist (this story, ε=0.38), originalist (separate story, ε ≈ 0.25), and legal positivist (separate story, ε ≈ 0.42). Each reading instantiates different extraction mechanisms, different beneficiary/victim structures, and different interpretive authorities. They are not the same constraint viewed from different angles — their ε values differ significantly because the coordination functions and extraction mechanisms are structurally distinct. All three stories link via network.affects_constraints because they compete for institutional authority over constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
