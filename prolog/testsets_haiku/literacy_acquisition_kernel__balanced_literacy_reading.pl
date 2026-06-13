% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Instruction (Phonics + Meaning Synthesis)
 *   domain: educational/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy emerged in the 2000s-2010s as a proposed resolution to
 *   the reading wars—the decades-long conflict between phonics advocates
 *   (citing cognitive science on phonological awareness) and whole-language
 *   advocates (citing engagement and motivation research). The
 *   balanced-literacy reading claims that reading acquisition requires BOTH
 *   systematic phonics instruction AND meaningful text engagement, and that
 *   the two are complementary rather than contradictory. It is positioned as
 *   evidence-based synthesis. However, the reading is contested: phonics
 *   advocates argue it dilutes the scientifically-established primacy of
 *   systematic phonics; whole-language advocates argue it reintroduces
 *   decontextualized skill-drill at the expense of meaning and motivation;
 *   structured-literacy advocates argue it lacks the intensity and
 *   explicitness required for learners with dyslexia. Meanwhile, school
 *   publishers and professional-development vendors have built substantial
 *   revenue streams around balanced-literacy curriculum adoption and
 *   retraining, creating structural incentives for the reading to persist
 *   even if it fails to achieve synthesis.
 *
 * KEY AGENTS:
 *   - balanced_literacy_advocates (organized academic/policy seat; agenda_setter) — set curriculum standards, design training, advocate adoption; claim genuine synthesis of valid insights
 *   - school_publishers (institutional; beneficiary) — sell curriculum packages and assessments; benefit from method-churn revenue and adoption cycles
 *   - teacher_professional_development_industry (organized; beneficiary) — deliver training and coaching; sustained demand created by complex dual-mode pedagogy
 *   - classroom_teachers (moderate power; payer + beneficiary) — implement dual pedagogy; bear labor and cognitive cost; may benefit if integration works
 *   - beginning_readers (powerless; beneficiary) — receive combined phonics + meaning instruction; outcomes depend on implementation fidelity and individual learner needs
 *   - struggling_readers_with_phonological_deficits (powerless; excluded) — likely underserved by relative de-emphasis on intensive phonics; voice absent from adoption discussions
 *   - phonics_advocates (organized; excluded) — argue for phonological primacy; research often reframed or downweighted in adoption narratives
 *   - whole_language_advocates (organized; excluded) — argue for meaning-engagement primacy; theoretical premises largely marginalized in policy discourse
 *   - curriculum_adoption_boards (institutional; agenda_setter) — select frameworks; under pressure from multiple constituencies; balanced literacy marketed as satisfying all
 *   - literacy_research_community (organized; observer) — debate what empirical evidence actually shows; disagreement about whether balanced literacy reflects synthesis or compromise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction (Phonics + Meaning Synthesis)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/cognitive_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '3ee8ef37-f81b-4466-b3a4-e0424840494c').
narrative_ontology:cs_kernel_codification('3ee8ef37-f81b-4466-b3a4-e0424840494c', distributed).
narrative_ontology:cs_authority_grounding('3ee8ef37-f81b-4466-b3a4-e0424840494c', distributed).
narrative_ontology:cs_reading_relation('3ee8ef37-f81b-4466-b3a4-e0424840494c', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ee8ef37-f81b-4466-b3a4-e0424840494c', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ee8ef37-f81b-4466-b3a4-e0424840494c', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('3ee8ef37-f81b-4466-b3a4-e0424840494c', foundational, phonics_and_meaning_both_necessary).
narrative_ontology:cs_axiom_status(phonics_and_meaning_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3ee8ef37-f81b-4466-b3a4-e0424840494c', phonics_and_meaning_both_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3ee8ef37-f81b-4466-b3a4-e0424840494c', foundational, complementarity_over_competition).
narrative_ontology:cs_axiom_status(complementarity_over_competition, holdable).
narrative_ontology:cs_axiom_grounding('3ee8ef37-f81b-4466-b3a4-e0424840494c', complementarity_over_competition, conventional).
narrative_ontology:cs_reference_frame('3ee8ef37-f81b-4466-b3a4-e0424840494c', integrated_reading_systems).
narrative_ontology:cs_drift_state('3ee8ef37-f81b-4466-b3a4-e0424840494c', contemporary_evidence_contest, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3ee8ef37-f81b-4466-b3a4-e0424840494c', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, school_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_professional_development_industry).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_adoption_boards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, beginning_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, parents_of_beginning_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Literacy scholars and education policymakers who propose balanced literacy as the resolution to the reading wars. They set curriculum standards, design teacher training programs, and advocate for adoption in school districts. They argue the approach synthesizes genuine insights from both phonics and whole-language traditions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_advocates, agenda_setter,
    organized, biographical, mobile, national).

% Implement balanced literacy in classrooms through adopted curriculum materials and professional development. They bear the labor cost of managing dual instructional modes (explicit phonics blocks plus meaning-centered reading), the cognitive load of managing competing pedagogical frameworks, and the compliance burden of demonstrating adherence to district-mandated balance. They also benefit from a pedagogically coherent framework that, if it works, resolves the tension they experience between two partially valid approaches.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Sell balanced literacy curriculum packages, workbooks, and assessment tools to school districts. The reading wars ensure sustained demand for curriculum revision: each methodological turn creates new adoption cycles. Balanced literacy, positioned as the synthesis, is presented as cutting-edge and evidence-based, justifying premium pricing and regular updates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, school_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Delivers training programs, coaching, and certification in balanced literacy methods. The complexity of implementing two distinct instructional systems creates continuous demand for professional development, consulting, and coaching services. Each shift in the reading wars opens new consulting and training revenue streams.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_professional_development_industry, beneficiary,
    organized, generational, arbitrage, national).

% Receive reading instruction that combines explicit phonics skill-building with engagement in meaningful connected texts. If the balance is achieved, they benefit from both decoding support and motivation to read. If the balance is poorly calibrated or insufficiently implemented, they receive fragmented or under-emphasized components of either system.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, beginning_readers, beneficiary,
    powerless, immediate, trapped, local).

% Are the population most benefited by explicit, systematic phonics instruction (structured literacy tradition). Balanced literacy's emphasis on meaning-engagement and naturalistic phonics acquisition may under-serve their specific need for intensive, explicit decoding instruction. Their voice is often absent from curriculum adoption discussions dominated by typically-developing reader needs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers_with_phonological_deficits, excluded,
    powerless, immediate, trapped, local).

% Literacy scientists and education researchers who argue for explicit, systematic phonics as primary. They contend balanced literacy underweights phonological foundations and spreads resources across incoherent methods rather than optimizing for what the cognitive science establishes. Their research and testimony are often reframed or selectively cited in balanced literacy adoption narratives.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, phonics_advocates, excluded,
    organized, biographical, mobile, national).

% Literacy educators and child development researchers who argue for emergent reading through meaningful text engagement. They contend balanced literacy reintroduces the decontextualized skill drill that damages reading motivation and returns to a deficit-oriented framework. Their theoretical premises are largely marginalized in contemporary policy discourse.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_advocates, excluded,
    organized, biographical, mobile, national).

% State and district education officials adopt curriculum frameworks and reading programs. They face pressure from multiple constituencies, including parents concerned about reading outcomes, teachers seeking clarity and tools, and publishers offering evidence-based solutions. Balanced literacy is marketed as the science-supported synthesis that can satisfy diverse demands.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_adoption_boards, agenda_setter,
    institutional, biographical, constrained, regional).

% Cognitive scientists, educational researchers, and literacy scholars studying reading acquisition. They observe the reading wars from the vantage of empirical evidence and debate what that evidence actually shows. Some attest balanced literacy reflects an honest synthesis of disparate findings; others argue it is a pragmatic compromise that satisfies neither the phonological science nor the motivational/engagement science.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_research_community, observer,
    organized, generational, mobile, global).

% Want their children to learn to read fluently and with enjoyment. They delegate instruction to schools but monitor progress. They may experience confusion if school messaging about method keeps changing or if home reading support and school method are misaligned. Balanced literacy, marketed as evidence-based synthesis, offers reassurance that schools are addressing both skill and engagement.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, parents_of_beginning_readers, beneficiary,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, school_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the reading wars by proposing that reading acquisition requires BOTH systematic phonics AND meaningful text engagement—neither alone is sufficient; the coordination problem is how to integrate two partially valid but seemingly contradictory instructional systems into classroom practice without losing the benefits of either.
% TRANSFER_FUNCTION: Moves revenue from school budgets to curriculum publishers and professional development vendors through adoption cycles, retraining, and continuous material replacement. The balanced literacy framework justifies sustained demand for new curriculum packages, coaching, and assessments by positioning the integrated approach as cutting-edge, evidence-based pedagogy requiring continuous refinement.
% ABSENT_VOICES: Struggling readers with significant phonological deficits (who need intensive, explicit structured literacy) and whole-language advocates (whose theoretical premises are largely delegitimized in contemporary policy discourse). Their absence means the curriculum adoption process does not include testimony from those who might argue balanced literacy underserves very-low-decoding populations or oversells the evidence for meaning-centered phonics emergence.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared, schools would likely return to methodological oscillation: oscillating between phonics-first and meaning-centered pedagogies as research trends and political winds shift. The disappearance would not eliminate reading instruction—it would eliminate the particular synthesis claim that frames the reading wars as resolvable. Alternatively, if the reading wars were genuinely resolved by the balanced approach, its disappearance would cause outcomes to degrade (suggesting it was real coordination); if it was a repackaged compromise satisfying neither camp, disappearance would reveal that underlying tensions persist.
% FOUNDING_PROBLEM: The reading wars of the 1990s-2010s: cognitive science research into phonological awareness and decoding (the Science of Reading movement) contradicted prevailing whole-language pedagogy; schools faced irreconcilable demands from phonics advocates citing cognitive science and whole-language advocates citing motivation and engagement research. Balanced literacy was positioned as the resolution: both systems are valid, both address real learning needs, instruction must incorporate both.
% FOUNDING_PROBLEM_CORROBORATION: Balanced literacy advocates (agenda_setter seat) attest the reading wars are unresolved and balance is necessary. Phonics advocates (represented in phonics_research_community) attest cognitive science shows systematic phonics is primary and balanced approaches dilute it. Whole-language advocates attest meaning-engagement is primary and explicit phonics harms motivation. Outcome data is mixed: studies show balanced literacy works for typical learners but may under-serve struggling readers and produces different outcomes under different implementation fidelities. No neutral third party outside the benefiting seats (school publishers, PD industry, balanced advocates themselves) has established which reading's account is correct; the founding problem status remains contested because the underlying research disagreement persists.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because balanced literacy does solve a real coordination problem—it provides teachers and schools with a coherent framework that acknowledges both phonological and engagement science, reducing the decision paralysis of the reading wars. However, extractiveness is not low because the framework's adoption also depends on ongoing curriculum replacement, professional development revenue, and the sustained perception that methodological balance requires continuous refinement. The theater ratio (0.48) is near-symmetric: there is a genuine instructional practice being implemented (not purely performative), but the marketing narrative (evidence-based synthesis) often exceeds what the empirical data supports, and the adoption messaging sometimes obscures genuine disagreements about mechanism. Suppression is moderate (0.42) because balanced literacy does not require coercive enforcement—it is adopted through standard education policy processes. However, the adoption process suppresses (by excluding from curriculum discourse) the voices of structured-literacy advocates (who argue balanced literacy lacks intensity for struggling readers) and whole-language advocates (whose theoretical premises are treated as outdated). Accessibility collapse is moderate (0.62): teachers do have alternatives (they could adopt high-fidelity phonics-first or structured-literacy systems, or continue with meaning-centered approaches), but the policy pressure for balanced adoption and the substantial curriculum/PD investment make alternatives costly. Resistance is high (0.71): phonics and whole-language research communities continue to contest the reading, and practitioners report confusion about implementation; the constraint persists despite substantial resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   School publishers and the PD industry are the structural beneficiaries (d toward 0.0): they collect revenue from adoption cycles and retraining, their exit options are arbitrage (they can pivot to other educational products), and their interests align with sustained perception of methodological flux. Teachers are dual-positioned (secondary roles: payer + beneficiary): they bear the labor cost of managing two instructional systems, but they also benefit if the integration actually reduces the cognitive conflict they experience between phonological and engagement imperatives. Beginning readers are beneficiaries (role: beneficiary) if the balance works, trapped in local contexts without genuine exit; they are also potentially victims if their learning needs are not served by the particular balance achieved. Struggling readers with phonological deficits and both sets of excluded advocates (phonics and whole-language) experience suppression of their voice but are not direct payers—they are excluded rather than extraction targets. The directionality asymmetry is that school publishers and PD vendors have clear beneficiary directionality while benefiting teachers have constrained exit and are partly captured as payers (labor cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading wars; conflicting science) has a contested status rather than clear resolution. If the founding problem is declared dead (reading wars resolved by synthesis), the constraint would need to demonstrate that balanced-literacy classrooms produce superior outcomes to both pure approaches across diverse learners—evidence is currently mixed and contested. If the founding problem is declared live (methods still disputed), then balanced literacy's claim to be the synthesis is questioned—it becomes a pragmatic oscillation-management tool rather than a resolution. If the founding problem is declared contested (which is most accurate), then balanced literacy sits in a stable but fragile position: it can only persist as long as the underlying disagreement remains sufficiently unsettled that both camps accept the synthesis framing as preferable to oscillation. The mandatrophy scenario is that balanced literacy is a zombie constraint: it persists because neither the phonics camp nor the whole-language camp has sufficient power to enforce their reading, so the balanced compromise becomes the default adoption framework, even though neither camp is satisfied. Theater ratio of 0.48 and extractiveness of 0.58 are consistent with this: the constraint performs real coordination (managing the reading wars) but also rides on extractive revenue that benefits from the wars' continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_vs_compromise_ambiguity,
    'Is balanced literacy a genuine synthesis of complementary instructional systems, or a pragmatic compromise that satisfies neither camp and obscures real theoretical disagreements?',
    'Longitudinal outcome studies comparing balanced literacy implementation to high-fidelity phonics-first and high-fidelity meaning-centered approaches, controlling for implementation quality and student population characteristics. If balanced implementation produces outcomes equivalent to both pure approaches across diverse learners, it is genuine synthesis; if it underperforms both on their respective outcome domains (phonological decoding vs. reading motivation/engagement), it is compromise.',
    'If genuine synthesis: balanced literacy legitimately resolves the reading wars and represents stable pedagogical knowledge. If compromise: the underlying disagreement persists, and the framework is extractive—perpetuating the methodological oscillation by rebranding previous conflicts as harmonized. This determines whether the constraint is durable coordination or temporary theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_vs_compromise_ambiguity, empirical, 'Whether balanced literacy achieves true pedagogical synthesis or merely manages competing claims.').

omega_variable(
    reading_kernel_kernel_vs_reading_operationalization,
    'What is the actual kernel being contested—is it the nature of reading acquisition itself (cognitive-developmental), or is it the operationalization of ''balance'' in classroom practice?',
    'Discourse analysis of sibling reading positions: do the four readings (balanced, phonics, whole-language, structured-literacy) dispute the mechanism of reading acquisition, or do they dispute how classroom time and resources should be allocated? If the dispute is about mechanism, balanced literacy claims a substantive cognitive insight; if about operationalization, it is a resource-allocation compromise.',
    'If the kernel is mechanism, balanced literacy''s claim that both phonics and meaning are necessary carries substantial epistemic weight. If the kernel is operationalization, the synthesis claim is pragmatic rather than scientifically grounded, and the reading can be reframed as orchestrating class-consensus around a shared fiction rather than discovering a true property of reading cognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_kernel_vs_reading_operationalization, conceptual, 'Whether the reading wars concern reading cognition or classroom resource allocation.').

omega_variable(
    beneficiary_capture_and_method_churn,
    'To what extent does the published/PD industry actively perpetuate the reading wars and balanced-literacy adoption cycles to sustain revenue, versus responding to genuine pedagogical uncertainty?',
    'Market analysis of curriculum adoption cycles and publishing revenue streams; interview studies of curriculum adoption decision-makers examining whether methodological trends correlate with evidence updates or with product release cycles; longitudinal tracking of which research gets highlighted in marketing versus which gets ignored.',
    'High capture would establish balanced literacy as extractive—the method-churn revenue depends on sustained perception of methodological flux. Low capture would support a reading of balanced literacy as honest response to genuine scientific uncertainty. The distinction determines whether the constraint is rent-seeking (snare-flavored) or coordination-under-uncertainty (rope-flavored).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_and_method_churn, empirical, 'Whether methodological oscillation is market-driven or evidence-driven.').

omega_variable(
    struggling_reader_exclusion_and_victims,
    'Are struggling readers with phonological deficits or other decoding challenges systematically underserved by balanced literacy''s relative de-emphasis on intensive systematic phonics compared to structured-literacy approaches?',
    'Comparative outcome studies for subpopulations: matched cohorts of struggling readers receiving balanced literacy vs. structured-literacy instruction, with outcome measures on both decoding automaticity and reading enjoyment/engagement. If struggling readers show superior decoding outcomes under structured-literacy but equivalent engagement under balanced, balanced literacy is extractive for that population (victims field).',
    'If yes, struggling readers with phonological deficits constitute a victim class—the constraint satisfies typical learners while extracting from the population that needs the most intensive support. If no, balanced literacy genuinely serves all learners. This determines whether the constraint generates unequal outcomes that might require systemic correction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(struggling_reader_exclusion_and_victims, empirical, 'Whether balanced literacy adequately serves struggling decoders.').

omega_variable(
    kernel_committer_ambiguity_reading_distinctness,
    'Is balanced-literacy-reading a genuinely distinct reading of the literacy-acquisition kernel, or is it functionally a rebranding/repositioning of whole-language pedagogy under the guise of science?',
    'Linguistic and conceptual analysis: trace whether balanced literacy''s axioms (both-systems-necessary, complementary, integrated) were present in whole-language scholarship pre-reading-wars, or whether they emerged post-hoc in response to phonological science. If present pre-hoc, balanced literacy is a sibling reading coexisting with whole-language. If post-hoc rebranding, it is whole-language-reading with a science-appeal overlay, and should be collapsed into a single reading with an internal identity-shift omega.',
    'If distinct reading: four-way contest among four coherent positions. If rebranded: three-way contest (phonics, whole-language-now-called-balanced, structured-literacy), with the balanced reading''s apparent third way being a strategic repositioning. This affects whether the kernel contest is truly open (three or four viable readings) or whether balanced literacy''s adoption success represents capture of whole-language by phonological science rhetoric rather than genuine synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity_reading_distinctness, conceptual, 'Whether balanced literacy is a distinct reading or a rebranded version of whole-language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(lite_tr_t20, projected).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(lite_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(lite_be_t20, projected).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(lite_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(lite_su_t20, projected).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(lite_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy acquisition kernel admits four distinct readings: balanced-literacy-reading (this constraint) claims both phonological and meaning systems are necessary and complementary; phonics-reading emphasizes phonological primacy; whole-language-reading emphasizes meaning-emergence; structured-literacy-reading emphasizes systematic intensity across multiple domains. Each reading has distinct epsilon values, beneficiary structures, and contested status. Balanced literacy influences all three sibling readings by positioning itself as the synthesis; it coexists with phonics and whole-language (neither forecloses the others) and influences (but does not foreclose) structured-literacy by claiming comprehensiveness is achieved through balance rather than intensity. The ε-invariance principle: each reading measures the same underlying instructional system (reading pedagogy) but makes different claims about what constitutes reading acquisition. Balanced literacy's ε (0.58) reflects moderate extractiveness from method-churn revenue and beneficiary-seat adoption patterns. The sibling readings would carry different ε values reflecting their distinct beneficiary structures and contested empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
