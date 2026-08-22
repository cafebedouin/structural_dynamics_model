% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__hybrid_continuity_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality — Hybrid Continuity Reading (Substrate plus Reconstruction)
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The colloquial label 'Hebrew vitality' covers a contested kernel: what
 *   counts as a living Hebrew, and what produced it. This file authors ONE
 *   reading of that kernel as a clean, epsilon-invariant constraint — the
 *   hybrid continuity reading: unbroken liturgical transmission was a
 *   causally necessary enabler but insufficient for vernacular life; the
 *   revival required both the preserved substrate and deliberate
 *   reconstruction plus communal adoption. Per the epsilon-referent rule,
 *   extractiveness is authored for the standing arrangement under contest —
 *   the actual historical configuration of centuries of liturgical
 *   maintenance followed by the revival project — as THIS reading assesses
 *   it. By its own lights that configuration involved real costs (generations
 *   investing in a non-spoken register) which the reading prices as enabling
 *   investment rather than extraction; hence low epsilon. The sibling
 *   readings (ritual-continuity-is-vitality; only-native-generation-counts)
 *   are separate constraints in separate files, linked via
 *   network.affects_constraints. The claim and the metrics are independent
 *   authored facts: claimed_type reflects the reading's structural role in
 *   the field; the metrics describe its actual discursive operation. KEY
 *   AGENTS (by structural relationship): - comparative_sociolinguists:
 *   agenda-setter and beneficiary (institutional/mobile) — administers the
 *   synthesis through journals, handbooks, and curricula -
 *   diaspora_liturgical_communities: beneficiary (organized/identity_locked)
 *   — centuries of recited-and-studied Hebrew supply the substrate the
 *   reading credits - israeli_hebrew_establishment: beneficiary with payer
 *   residue (institutional/constrained) — completes vernacularization;
 *   accepts shared credit, guards revival primacy -
 *   hebrew_educators_ulpan_staff: payer with beneficiary residue
 *   (moderate/constrained) — implements the two-register curriculum -
 *   second_generation_revival_movements: beneficiary (organized/mobile) —
 *   import the synthesis as planning doctrine -
 *   traditionalist_liturgy_defenders: excluded (organized/identity_locked) —
 *   hold ritual completeness; absent from the standardizing venues
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.1).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.06).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality — Hybrid Continuity Reading (Substrate plus Reconstruction)").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '208bc30c-4ed5-4aeb-b00c-dfefa6820181').
narrative_ontology:cs_kernel_codification('208bc30c-4ed5-4aeb-b00c-dfefa6820181', distributed).
narrative_ontology:cs_authority_grounding('208bc30c-4ed5-4aeb-b00c-dfefa6820181', distributed).
narrative_ontology:cs_reading_relation('208bc30c-4ed5-4aeb-b00c-dfefa6820181', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('208bc30c-4ed5-4aeb-b00c-dfefa6820181', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('208bc30c-4ed5-4aeb-b00c-dfefa6820181', foundational, liturgical_transmission_causally_necessary).
narrative_ontology:cs_axiom_status(liturgical_transmission_causally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('208bc30c-4ed5-4aeb-b00c-dfefa6820181', liturgical_transmission_causally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('208bc30c-4ed5-4aeb-b00c-dfefa6820181', foundational, vernacularization_requires_deliberate_reconstruction).
narrative_ontology:cs_axiom_status(vernacularization_requires_deliberate_reconstruction, holdable).
narrative_ontology:cs_axiom_grounding('208bc30c-4ed5-4aeb-b00c-dfefa6820181', vernacularization_requires_deliberate_reconstruction, empirically_contingent).
narrative_ontology:cs_axiom('208bc30c-4ed5-4aeb-b00c-dfefa6820181', secondary, vitality_is_graded_across_functions).
narrative_ontology:cs_axiom_status(vitality_is_graded_across_functions, holdable).
narrative_ontology:cs_axiom_grounding('208bc30c-4ed5-4aeb-b00c-dfefa6820181', vitality_is_graded_across_functions, conventional).
narrative_ontology:cs_reference_frame('208bc30c-4ed5-4aeb-b00c-dfefa6820181', graded_functional_vitality_continuum).
narrative_ontology:cs_drift_state('208bc30c-4ed5-4aeb-b00c-dfefa6820181', contemporary_revival_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('208bc30c-4ed5-4aeb-b00c-dfefa6820181', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, comparative_sociolinguists).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, israeli_hebrew_establishment).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, second_generation_revival_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, hebrew_educators_ulpan_staff).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, israeli_hebrew_establishment).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, hebrew_educators_ulpan_staff).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, conjoint_substrate_reconstruction_model).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, intergenerational_transmission_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the journals, handbooks, and graduate seminars where accounts of Hebrew's revival are standardized. They weigh liturgical-transmission evidence against revival-era documentation and certify which explanations enter the textbooks. Their professional standing rides on the synthesis holding together; switching frameworks would mean reworking syllabi and review networks, which they could do without leaving the field.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_sociolinguists, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, comparative_sociolinguists, beneficiary).

% Maintained Hebrew as a recited and studied language across centuries of dispersion, producing the textual competence that later revival drew on. The synthesis credits that maintenance as a load-bearing contribution rather than a failed everyday-speech habit. Leaving the practice would mean leaving the communal identity it anchors, so participation is not experienced as optional.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, diaspora_liturgical_communities, beneficiary,
    organized, civilizational, identity_locked, global).

% The Academy, universities, and school system that completed vernacularization: coining terminology, setting norms, and teaching mass literacy in speech. The synthesis assigns them the reconstruction half of the story while denying them sole authorship of the outcome. They accept the shared credit in official histories while guarding the primacy-of-revival narrative inside national institutions.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, israeli_hebrew_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, israeli_hebrew_establishment, payer).

% Teach both registers daily: classical texts as inherited material, modern speech as target competence. The synthesis dictates their curriculum architecture. They carry the implementation workload and absorb criticism from both directions — traditionalists who want more liturgy in the classroom, purists who want less.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, hebrew_educators_ulpan_staff, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, hebrew_educators_ulpan_staff, beneficiary).

% Welsh, Maori, Gaelic, and comparable programs that import the Hebrew lesson: secure the inherited register AND build new spoken domains. They consume the synthesis as planning doctrine and report program outcomes back into the scholarly literature that produced it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, second_generation_revival_movements, beneficiary,
    organized, generational, mobile, continental).

% Circles, largely outside the journals and ministries where the synthesis is administered, who hold that recited Hebrew is complete in itself and object to any framing of liturgy as raw material for a secular vernacular. They publish in their own venues and do not participate in the sociolinguistic conversation that standardized the hybrid account.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, traditionalist_liturgy_defenders, excluded,
    organized, civilizational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared explanatory frame that lets philological scholarship on liturgical transmission and revival historiography occupy one account: preserved textual competence supplied the input, deliberate lexical and structural reconstruction plus communal adoption converted it into speech. Stated without evaluation, this is the coordination problem the arrangement solves — reconciling two bodies of evidence and two professional communities that otherwise talk past each other.
% TRANSFER_FUNCTION: Moves interpretive authority toward the scholarly synthesis and distributes practical templates outward: curriculum legitimacy flows to educators, planning doctrine flows to revival movements abroad, and communal dignity flows to diaspora liturgical communities whose maintenance is retroactively priced as contribution. Attention and archival labor flow inward, toward documenting both the transmission record and the revival-era coinage decisions.
% ABSENT_VOICES: Traditionalist liturgy defenders would object that the frame instrumentalizes sacred speech by demoting it to 'substrate'; they are structurally absent from the journals, ministries, and conferences where the synthesis is standardized, publishing instead in separate religious venues. Hard-line revival purists who deny the liturgical past any causal role are likewise present only at the margins of the sociolinguistic conversation. Both objections circulate outside the rooms where the account hardened into textbook orthodoxy.
% DISAPPEARANCE_RATIONALE: If the hybrid synthesis vanished overnight, the Hebrew language itself would continue unchanged, but the scholarly and practitioner world would rearrange: curricula premised on the two-register architecture would lose their warrant, revival movements importing the Hebrew template would lose their doctrinal anchor, and the field would revert to the liturgical-versus-native standoff that the synthesis currently mediates. The arrangements of every named seat depend on the account holding.
% FOUNDING_PROBLEM: The arrangement was built to solve an explanatory puzzle: why Hebrew alone among heritage and liturgical languages achieved vernacular rebirth, what causal weight each inheritance carried, and — practically — what a community must secure to repeat the achievement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the Hebrew-studies beneficiary set: cross-case revitalization outcomes (limited Irish-language recovery despite state schooling, Welsh and Maori domain-building results, Manx and Cornish reconstruction-from-documentation efforts) are routinely cited in the general revitalization literature as independent tests of the substrate-plus-reconstruction claim, and Fishman-line intergenerational-transmission research corroborates the adoption half from a body of work not centered on Hebrew. Notably, traditionalist liturgical circles do NOT corroborate the frame — they reject its premises outright — which is itself signal that the synthesis is a scholarly construction rather than a consensus of all affected parties.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.10: the arrangement under contest, priced by this reading's own lights, transfers little from anyone to anyone; the residual epsilon reflects boundary-work — rival accounts getting footnoted rather than engaged, and the synthesis collecting citational rent as the default frame. Suppression 0.06: sibling readings publish freely; the only enforcement is mild editorial gatekeeping, and suppression is authored as the raw structural property it is (unscaled by directionality or scope — the engine owns any scaling of extractiveness alone). Theater ratio 0.14: growing share of invocations are formulaic — grant proposals and curricula citing the synthesis ritually without engaging substrate data — but the underlying explanatory function is still performed. Accessibility collapse 0.30: understanding the synthesis does not close the alternatives; both siblings remain fully arguable, which is precisely what distinguishes this reading from a natural-law claim. Resistance 0.35: sustained pushback from both flanks keeps the account defended rather than self-evident. Measurement series run on one shared grid (T0-T60, approximately 1960-2020) with all three tracked metrics authored at every point; the gentle extractiveness arc tracks canonization (peak influence, peak citational rent) followed by stabilization, and the slow theater rise tracks formulaic adoption. No suppression_requirement trend is asserted beyond mild editorial variation because enforcement capacity was never the dynamic this story traces.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the sociolinguist seat the synthesis is a liberating coordination device that ended an unproductive binary quarrel. From the Israeli establishment seat it is mixed: genuine credit for the reconstruction half, paired with dispossession of the sole-authorship narrative the national institutions tell about themselves. From the diaspora liturgical seat it is vindication — centuries of maintenance retroactively priced as contribution. From the traditionalist seat it is erasure: a frame that converts sacred speech into raw material. Same text, four different operative realities; the engine derives this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared and no victims exist, so the derivation places every seated agent near the beneficiary end of directionality — correct for this reading, which names no paying class. The Israeli establishment's ambivalence (beneficiary role, reputational cost of losing narrative exclusivity) does not warrant an override: its cost is reputational, not material, and the derived low d captures its net position. Educators bear real labor costs but diffuse ones, handled by their payer role rather than by victim declaration. No directionality_overrides are authored because the derivation chain already produces the right relationships from the declared structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabels. Reading the contested terrain as conflict-over-resources would fabricate a snare where no victim class exists — dissenters lose argument space, not livelihoods. Conversely, the rising theater ratio could tempt a piton reading, but the test fails: the explanatory function is still performed, the founding problem is live, and fixing (dropping the synthesis) would be cheap precisely because nothing captive depends on it. On the R5 mismatch consumer: founding_problem_status=live crossed with disappearance_verdict=world_rearranges yields no capture-or-zombie flag, and the corroboration comes substantially from outside the beneficiary set (cross-case revitalization outcomes, transmission research), so the genealogy is not a self-attesting origin myth. Mandatrophy is not resolved because the mandate has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_delta,
    'Is this hybrid synthesis a structurally distinct constraint, or a reframing that should inherit the liturgical_reading''s or native_daily_reading''s beneficiary/victim structure?',
    'Compare compiled classifications across the three sibling stories: if the hybrid story computes materially different effective extraction and seat divergence than either sibling, it is distinct; if it collapses onto a sibling''s profile, treat it as reframing and prune the family.',
    'If mere reframing, this story''s low epsilon is redundant with a sibling and the triadic family reduces to two constraints; if distinct, the family stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position_delta, conceptual, 'Whether the hybrid reading is a distinct constraint or a reframe of a sibling reading.').

omega_variable(
    substrate_composition_ambiguity,
    'How much of the revived vernacular''s structure descends from liturgical and biblical Hebrew versus European-language transfer (Yiddish, German, Russian, English) routed through the speech of revival-era speakers?',
    'Comparative typological analysis of revival-era corpora against Mishnaic and biblical norms and against contemporaneous Yiddish and German structures; acquisition-order studies of the first native generations.',
    'If transfer dominates, the liturgical-necessity axiom weakens toward ''any literate register suffices,'' pulling this reading toward reconstruction-heavy accounts and raising the salience of the native_daily_reading''s discontinuity claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_composition_ambiguity, empirical, 'Causal weighting of Hebraic substrate versus European transfer in the revived vernacular.').

omega_variable(
    counterfactual_without_liturgy,
    'Could a vernacular have been built from scriptural study alone had continuous liturgical use lapsed — that is, is ''necessary'' in the founding claim literal?',
    'Cross-case comparison with heritage languages whose liturgical use thinned (Samaritan Hebrew''s trajectory, Judeo-Aramaic decay) and with successful revivals lacking a deep liturgical substrate (Maori, Welsh).',
    'A yes collapses the necessity axiom and recasts liturgy as one sufficient channel among several; a no hardens the hybrid reading''s foundational claim and raises its resistance profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_without_liturgy, empirical, 'Counterfactual status of the liturgical-necessity claim.').

omega_variable(
    vitality_definition_reframe,
    'Does defining vitality as graded and functional — this reading''s reconciliation move — genuinely resolve the kernel contest, or does it change the subject relative to the binary definitions the sibling readings argue over?',
    'Test whether sibling partisans accept the graded definition without remainder: survey the disputed points (native acquisition, ritual continuity) to determine whether any live dispute survives translation into graded terms.',
    'If the reframe changes the subject, the hybrid reading resolves nothing and the kernel remains effectively binary-contested, with classification consequences residing in the siblings; if it translates, the triadic family is stable as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_definition_reframe, conceptual, 'Whether the graded-vitality reframe resolves or merely relocates the kernel dispute.').

omega_variable(
    template_generalizability,
    'Is substrate-plus-reconstruction a general recipe for language revitalization, or an idiosyncratic product of Hebrew''s unusual conditions (dispersed literate diaspora, ideological convergence, subsequent statehood)?',
    'Systematic outcome comparison across revival programs that adopted the template with varying substrate depth and reconstruction intensity, controlling for state support and community size.',
    'If idiosyncratic, the reading''s coordination value drops to case-description and its classification softens toward inertial; if general, template export continues and the coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(template_generalizability, empirical, 'Generalizability of the substrate-plus-reconstruction recipe beyond Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebvit_hybrid_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebvit_hybrid_tr_t12, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(hebvit_hybrid_tr_t24, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(hebvit_hybrid_tr_t36, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 36, 0.11).
narrative_ontology:measurement(hebvit_hybrid_tr_t48, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement(hebvit_hybrid_tr_t60, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 60, 0.14).

% Extraction over time
narrative_ontology:measurement(hebvit_hybrid_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(hebvit_hybrid_be_t12, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 12, 0.09).
narrative_ontology:measurement(hebvit_hybrid_be_t24, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 24, 0.11).
narrative_ontology:measurement(hebvit_hybrid_be_t36, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 36, 0.12).
narrative_ontology:measurement(hebvit_hybrid_be_t48, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 48, 0.11).
narrative_ontology:measurement(hebvit_hybrid_be_t60, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 60, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(hebvit_hybrid_su_t0, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(hebvit_hybrid_su_t12, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 12, 0.05).
narrative_ontology:measurement(hebvit_hybrid_su_t24, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 24, 0.06).
narrative_ontology:measurement(hebvit_hybrid_su_t36, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 36, 0.07).
narrative_ontology:measurement(hebvit_hybrid_su_t48, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 48, 0.07).
narrative_ontology:measurement(hebvit_hybrid_su_t60, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 60, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Hebrew vitality' conflates three structurally distinct claims — that ritual continuity constitutes vitality (liturgical_reading), that only native daily generation constitutes vitality (native_daily_reading), and that vitality emerged from substrate-plus-reconstruction (this file). Each reading instantiates a different constraint with its own epsilon, beneficiary structure, and failure modes; forcing one story to span all three would make epsilon observer-relative. Causal linkage runs upstream from the liturgical complex (the transmitted substrate) through this synthesis to the native-generation outcome the third reading isolates; the hybrid reading is cited by both siblings' partisans as either the referee or the evasion, which is why both edges are declared.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
