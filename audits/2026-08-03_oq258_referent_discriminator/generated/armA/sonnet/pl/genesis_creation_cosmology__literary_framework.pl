% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__literary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__literary_framework, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: genesis_creation_cosmology__literary_framework
 *   human_readable: Genesis 1-2 as Literary-Framework (ANE Cosmological Schema, Non-Cosmological)
 *   domain: religious_studies/theology/philosophy_of_science
 *
 * SUMMARY:
 *   The literary-framework reading holds that Genesis 1-2 deploys the shared
 *   cosmological vocabulary of the Ancient Near East (a solid firmament,
 *   waters above and below, ordered days of divine work) not to make claims
 *   about the physical universe's origin or age, but as literary-theological
 *   polemic against neighboring cosmogonies — asserting the God of Israel's
 *   sovereignty using the cosmological idiom every ANE audience already
 *   understood. This displaces both young-earth literalism (the text is not
 *   making the historical-scientific claims that reading requires) and, more
 *   subtly, standard theistic-evolution accommodationism (the text is not
 *   even making claims that need reconciling with evolutionary science — it
 *   was never in the business of cosmological description at all). The
 *   reading is attractive precisely because it resolves the faith-science
 *   conflict by declaring the conflict a category error, which is also what
 *   makes it structurally convenient for institutions under pressure to avoid
 *   that conflict.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholars: agenda_setter/beneficiary (institutional/arbitrage) — the reading is career and disciplinary infrastructure
 *   - mainline_denominational_leadership: beneficiary (institutional/mobile) — avoids institutional conflict with science, retains educated members
 *   - science_compatible_apologetics_movement: beneficiary (organized/mobile) — builds an audience niche on the reconciliation product
 *   - literalist_lay_congregants: payer (powerless/constrained) — inherited reading reframed without their participation
 *   - young_earth_institutions: payer (organized/constrained) — lose cultural and donor legitimacy as this reading gains institutional ground
 *   - ane_comparative_specialists: observer (analytical/analytical) — document the shared literary schema independent of theological payoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__literary_framework, 0.28).
domain_priors:suppression_score(genesis_creation_cosmology__literary_framework, 0.35).
domain_priors:theater_ratio(genesis_creation_cosmology__literary_framework, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, extractiveness, 0.28).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(genesis_creation_cosmology__literary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__literary_framework, piton).
narrative_ontology:human_readable(genesis_creation_cosmology__literary_framework, "Genesis 1-2 as Literary-Framework (ANE Cosmological Schema, Non-Cosmological)").
narrative_ontology:topic_domain(genesis_creation_cosmology__literary_framework, "religious_studies/theology/philosophy_of_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__literary_framework, '9b97a82f-883b-483f-ada4-466dea41c6db').
narrative_ontology:cs_kernel_codification('9b97a82f-883b-483f-ada4-466dea41c6db', fixed_text).
narrative_ontology:cs_authority_grounding('9b97a82f-883b-483f-ada4-466dea41c6db', expertise).
narrative_ontology:cs_interpretation_layer_present('9b97a82f-883b-483f-ada4-466dea41c6db').
narrative_ontology:cs_reading_relation('9b97a82f-883b-483f-ada4-466dea41c6db', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_reading_relation('9b97a82f-883b-483f-ada4-466dea41c6db', genesis_creation_cosmology__theistic_evolution, influences).
narrative_ontology:cs_axiom('9b97a82f-883b-483f-ada4-466dea41c6db', foundational, text_makes_no_cosmological_claims).
narrative_ontology:cs_axiom_status(text_makes_no_cosmological_claims, holdable).
narrative_ontology:cs_axiom_grounding('9b97a82f-883b-483f-ada4-466dea41c6db', text_makes_no_cosmological_claims, conventional).
narrative_ontology:cs_axiom('9b97a82f-883b-483f-ada4-466dea41c6db', secondary, ane_schema_is_purely_rhetorical_device).
narrative_ontology:cs_axiom_status(ane_schema_is_purely_rhetorical_device, holdable).
narrative_ontology:cs_axiom_grounding('9b97a82f-883b-483f-ada4-466dea41c6db', ane_schema_is_purely_rhetorical_device, empirically_contingent).
narrative_ontology:cs_reference_frame('9b97a82f-883b-483f-ada4-466dea41c6db', historical_critical_ane_comparative_method).
narrative_ontology:cs_drift_state('9b97a82f-883b-483f-ada4-466dea41c6db', post_1960s_biblical_archaeology_boom, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9b97a82f-883b-483f-ada4-466dea41c6db', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, mainline_denominational_leadership).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__literary_framework, science_compatible_apologetics_movement).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, literalist_lay_congregants).
narrative_ontology:constraint_victim(genesis_creation_cosmology__literary_framework, young_earth_institutions).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__literary_framework, text_critical_method_authority).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__literary_framework, compatibilism_between_faith_and_science).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comparative ANE philology (Enuma Elish, Baal Cycle, Atrahasis) is the tool they use to reframe Genesis 1-2 as literary-theological polemic rather than physical description. This reading is career-load-bearing: journal placement, tenure cases, and seminary curricula run through it. They set the terms of what counts as a serious reading of the text and can move freely between confessional and secular academic spaces.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, academic_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__literary_framework, academic_biblical_scholars, beneficiary).

% Adopting the literary-framework reading lets denominations avoid public conflict with mainstream science, retain educated congregants who would otherwise leave over young-earth claims, and present the tradition as intellectually respectable. They administer this reading through seminaries and official statements without needing to enforce it congregation by congregation.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, mainline_denominational_leadership, beneficiary,
    institutional, generational, mobile, national).

% Writers, podcasters, and campus ministries built around reconciling faith and science use this reading as their core product. It gives them an audience of scientifically literate believers and a market niche distinct from both secular skepticism and young-earth creationism.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, science_compatible_apologetics_movement, beneficiary,
    organized, biographical, mobile, national).

% Raised to read the days of creation as historical narrative, they experience the literary-framework reading as their tradition being reinterpreted out from under them by credentialed authorities they cannot contest on the same terms. Exit means either accepting a reading that feels like doctrinal loss, leaving the interpretive community entirely, or retreating to explicitly young-earth congregations — none of which restores the reading they inherited.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, literalist_lay_congregants, payer,
    powerless, biographical, constrained, local).

% Museums, seminaries, and publishing operations built on six-literal-day creation lose institutional legitimacy and funding as the literary-framework reading gains ground in mainstream seminaries and Christian higher education, ceding cultural terrain and donor bases to interpretations that treat their core claim as pre-critical.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, young_earth_institutions, payer,
    organized, generational, constrained, national).

% Study the shared literary DNA across Mesopotamian, Ugaritic, and Israelite cosmogonies without a confessional stake in which theological reading wins; they document the schema-borrowing itself, which is the evidentiary basis all three kernel readings must contend with differently.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__literary_framework, ane_comparative_specialists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way to hold the text as authoritative scripture while declining to make it answerable to modern cosmology or geology, avoiding a head-on collision between the tradition and settled science that would otherwise force a binary choice.
% TRANSFER_FUNCTION: Moves interpretive authority over the creation narrative from lay tradition and literal-reading communities to academically credentialed readers; moves institutional legitimacy and congregational allegiance from young-earth institutions toward mainline and academically-aligned communities.
% ABSENT_VOICES: Literalist congregants rarely participate in the scholarly conversation that produces this reading; their objection — that this reframes the text into something their inherited faith never was — is treated by proponents as a pastoral problem to be managed rather than a substantive claim to be adjudicated.
% DISAPPEARANCE_RATIONALE: If the literary-framework reading vanished, mainline institutions that rely on it to avoid science-conflict would face renewed pressure to either concede ground to young-earth readings or find another accommodationist strategy; academic biblical studies would lose a load-bearing paradigm in Genesis scholarship. Young-earth institutions would read its disappearance as vindication rather than loss. Whether 'the world rearranges' depends entirely on which seat is asked — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: How to retain Genesis 1-2 as scripturally authoritative once 19th-20th century geology, biology, and ANE archaeology made a straightforward historical-cosmological reading untenable for readers committed to mainstream science.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and religion outside any confessional stake (e.g. scholars of 19th-century geology's reception in Protestant institutions) corroborate that the accommodation problem is real and unresolved; the ANE comparative specialists corroborate the underlying philological claim (shared cosmological schema) independent of which theological use is made of it. No source outside the beneficiary set corroborates that the *literary-framework conclusion specifically* (rather than some rival accommodation) is the correct resolution — that remains a live theological judgment, not an external fact.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__literary_framework, contested).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__literary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__literary_framework, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_cosmology__literary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__literary_framework, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__literary_framework_tests).
:- end_tests(genesis_creation_cosmology__literary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) and rising slowly: the reading does not directly extract resources, but it does redistribute interpretive authority and institutional legitimacy away from literalist and young-earth communities toward academic and mainline institutions, and that redistribution has grown as the reading has become more institutionally entrenched in seminaries. Suppression (0.35) is real but soft — no one is coercively prevented from holding a literalist reading, but professional and institutional gatekeeping (peer review, seminary accreditation, respectable-discourse norms) makes the literalist alternative costly to hold in credentialed spaces. Theater ratio is notably high and rising (0.15 to 0.40 over the interval) because a growing share of the reading's public defense functions as intellectual respectability signaling — 'serious people don't read Genesis literally' — rather than as first-order argument about the text's genre, which is the actual coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the academic-scholar and mainline-leadership seats this looks like straightforward historical-critical progress: reading the text in its actual ancient context rather than imposing anachronistic scientism on it. From the literalist-congregant seat the same move looks like their tradition being reassigned a genre by people with institutional power to make that reassignment stick, with no equivalent power on their side to contest it. The engine should register this asymmetry structurally: same text, same historical facts about ANE cosmology, different exposure to the consequences of who gets to say what kind of claim the text is making.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars and mainline leadership sit near the beneficiary end: they gain professional and institutional legitimacy, and their exit options (arbitrage, mobile) reflect genuine freedom to move between confessional and secular framings as convenient. Literalist congregants sit near the target end: the reading is imposed on their inherited tradition by authorities they cannot out-credential, and their exit options are constrained (leaving the interpretive community costs them their community, not just their view). Young-earth institutions are targets in a different register — they lose ground competitively as an institutional field, not personally, hence organized/constrained rather than powerless/trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural authority with post-Enlightenment science) is genuinely live — it has not been resolved by fiat, it remains a real tension every heir to the tradition must navigate. That liveness is what keeps this from being a pure piton with no function at all: the coordination problem it solves (holding the text as authoritative without demanding readers deny geology) is real. But the rising theater_ratio signals that an increasing share of the reading's institutional maintenance is now about signaling scholarly respectability rather than doing the interpretive work the founding problem actually requires — which is the piton signature: the coordination kernel persists, but an increasing fraction of what keeps it running is performance rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_determination_authority,
    'Who has the legitimate authority to determine that Genesis 1-2 belongs to the genre ''theological polemic using borrowed cosmological schema'' rather than ''historical narrative'' or ''theologically-inflected but still cosmologically engaged narrative'' — and is that a properly historical-critical question or itself a theological commitment dressed as literary analysis?',
    'No purely empirical resolution exists; genre determination for ancient texts combines philological evidence (comparative ANE literature, internal literary markers) with interpretive framework commitments that are not themselves derivable from the philology. Convergence across confessionally diverse scholars using independent methods would strengthen the case; persistent disagreement tracking confessional commitment would weaken it.',
    'If genre determination is shown to be substantially theology-laden rather than a neutral historical-critical finding, the literary-framework reading''s claim to have displaced theological authority (rather than merely relocating it to a different theological camp) collapses, and it becomes structurally indistinguishable from theistic_evolution as a theological accommodation strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_determination_authority, conceptual, 'Whether genre classification is a neutral historical finding or a theological commitment in critical dress.').

omega_variable(
    committer_kernel_disagreement_locus,
    'Where exactly do the three sibling readings (literary_framework, theistic_evolution, young_earth_literal) locate their disagreement — is it about what the text says, what kind of claim the text is making, or what authority governs the answer to that question?',
    'This is not empirically resolvable from the text alone; the young_earth_literal reading locates authority in a plain-sense hermeneutic tradition, theistic_evolution locates it in a theological tradition open to non-literal reading of truth-claims, and literary_framework locates it in historical-critical/comparative philological method. Adjudicating between these requires a prior commitment about which interpretive authority governs scripture, which the text itself cannot settle.',
    'Recognizing the disagreement as located in authority-source rather than textual evidence explains why the three readings do not converge under additional archaeological or philological data — each already has an account of why the others'' evidence does not bind them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_locus, conceptual, 'The kernel dispute is located in competing authority structures, not competing readings of shared evidence.').

omega_variable(
    false_summit_academic_neutrality,
    'Does the academic-scholarly reading present itself as theologically neutral historical-critical method while in fact functioning as a beneficiary-serving displacement of competing theological authority?',
    'Track whether academic biblical scholars who adopt this reading disproportionately hold institutional positions (tenure, seminary appointments) that benefit from faith-science non-conflict, versus scholars reaching the same genre conclusion from institutions with no such stake.',
    'If adoption of the reading correlates strongly with institutional benefit rather than independent philological conviction, the reading functions less like discovered natural fact about the text''s genre and more like a constructed accommodation that happens to also serve its proponents — supporting a tangled_rope reading rather than a clean mountain-like historical-critical finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_academic_neutrality, empirical, 'Whether the reading''s claimed scholarly neutrality masks institutional self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__literary_framework, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1880, genesis_creation_cosmology__literary_framework, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(gene_tr_t1925, genesis_creation_cosmology__literary_framework, theater_ratio, 1925, 0.2).
narrative_ontology:measurement(gene_tr_t1961, genesis_creation_cosmology__literary_framework, theater_ratio, 1961, 0.25).
narrative_ontology:measurement(gene_tr_t1980, genesis_creation_cosmology__literary_framework, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(gene_tr_t2000, genesis_creation_cosmology__literary_framework, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(gene_tr_t2015, genesis_creation_cosmology__literary_framework, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(gene_tr_t2025, genesis_creation_cosmology__literary_framework, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1880, genesis_creation_cosmology__literary_framework, base_extractiveness, 1880, 0.1).
narrative_ontology:measurement(gene_be_t1925, genesis_creation_cosmology__literary_framework, base_extractiveness, 1925, 0.15).
narrative_ontology:measurement(gene_be_t1961, genesis_creation_cosmology__literary_framework, base_extractiveness, 1961, 0.18).
narrative_ontology:measurement(gene_be_t1980, genesis_creation_cosmology__literary_framework, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(gene_be_t2000, genesis_creation_cosmology__literary_framework, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(gene_be_t2015, genesis_creation_cosmology__literary_framework, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(gene_be_t2025, genesis_creation_cosmology__literary_framework, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(genesis_creation_cosmology__literary_framework, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__literary_framework, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology__young_earth_literal).
narrative_ontology:affects_constraint(genesis_creation_cosmology__literary_framework, genesis_creation_cosmology__theistic_evolution).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the genesis_creation_cosmology kernel. young_earth_literal reads the text as making literal historical-cosmological claims (high suppression against mainstream science, high resistance from without); theistic_evolution reads the text as making theological claims compatible with evolutionary cosmology (moderate accommodation, retains cosmological engagement); literary_framework (this story) denies the text engages cosmology as subject matter at all, treating the ANE schema as purely rhetorical. Each reading has its own ε, beneficiary/victim structure, and stakeholder set — they are not the same constraint measured three ways; they are three constraints sharing a contested text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
