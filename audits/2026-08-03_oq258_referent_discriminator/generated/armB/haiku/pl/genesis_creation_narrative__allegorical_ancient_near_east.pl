% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature (Allegorical Reading)
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   The allegorical reading of Genesis 1-2 emerged in nineteenth-century
 *   higher criticism and gained dominance in mainstream biblical scholarship
 *   and liberal theology through the twentieth century. It positions the
 *   creation account as Ancient Near Eastern mythopoetic literature — a
 *   theological statement about God's sovereignty and humanity's moral
 *   standing, not a factual cosmological chronicle. This reading decouples
 *   the text from adjudicative authority over science: believers can affirm
 *   both the text's theological truth and contemporary cosmology. The
 *   constraint operates by establishing this reading's hermeneutical
 *   canonical status in academic and mainline institutional contexts, which
 *   pressures religious communities toward allegorical interpretation and
 *   away from literalism. The cost falls on young-earth literalist
 *   communities who lose institutional standing and must defend cosmological
 *   claims against both scientific and scholarly consensus.
 *
 * KEY AGENTS:
 *   - Critical biblical scholars (organized beneficiary, power=organized) — sustain the allegorical reading through peer review, seminary teaching, and scholarly publication
 *   - Religious adherents seeking science compatibility (moderate beneficiary, power=moderate) — adopt the reading to gain cognitive coherence between faith and scientific worldview
 *   - Young-earth literalist communities (organized payer, power=powerful) — bear cost of institutional marginalization and must defend cosmological reading against consensus
 *   - Theistic evolutionary intermediates (dual beneficiary/payer, power=moderate) — gain science compatibility but lose theological specificity; must defend middle position
 *   - Theologians emphasizing dominion mandate (moderate payer, power=moderate) — lose normative force of creation account for environmental ethics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.38).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.22).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.38).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature (Allegorical Reading)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '4ad443ab-ec78-4201-a21e-11d77e56e41e').
narrative_ontology:cs_kernel_codification('4ad443ab-ec78-4201-a21e-11d77e56e41e', fixed_text).
narrative_ontology:cs_authority_grounding('4ad443ab-ec78-4201-a21e-11d77e56e41e', lineage).
narrative_ontology:cs_interpretation_layer_present('4ad443ab-ec78-4201-a21e-11d77e56e41e').
narrative_ontology:cs_reading_relation('4ad443ab-ec78-4201-a21e-11d77e56e41e', genesis_creation_narrative__literal_young_earth, coexists_with).
narrative_ontology:cs_reading_relation('4ad443ab-ec78-4201-a21e-11d77e56e41e', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('4ad443ab-ec78-4201-a21e-11d77e56e41e', foundational, text_genre_is_mythopoetic_not_factual).
narrative_ontology:cs_axiom_status(text_genre_is_mythopoetic_not_factual, holdable).
narrative_ontology:cs_axiom_grounding('4ad443ab-ec78-4201-a21e-11d77e56e41e', text_genre_is_mythopoetic_not_factual, empirically_contingent).
narrative_ontology:cs_axiom('4ad443ab-ec78-4201-a21e-11d77e56e41e', foundational, theological_meaning_independent_of_cosmological_accuracy).
narrative_ontology:cs_axiom_status(theological_meaning_independent_of_cosmological_accuracy, holdable).
narrative_ontology:cs_axiom_grounding('4ad443ab-ec78-4201-a21e-11d77e56e41e', theological_meaning_independent_of_cosmological_accuracy, deontological).
narrative_ontology:cs_reference_frame('4ad443ab-ec78-4201-a21e-11d77e56e41e', biblical_text_as_ancient_near_eastern_literature).
narrative_ontology:cs_drift_state('4ad443ab-ec78-4201-a21e-11d77e56e41e', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ad443ab-ec78-4201-a21e-11d77e56e41e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, religious_adherents_seeking_science_compatibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary_intermediates).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationist_institutional_networks).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary_intermediates).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, theologians_emphasizing_dominion_mandate).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, mythopoeic_hermeneutics_as_valid_scriptural_reading).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, theological_meaning_independent_of_cosmological_accuracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars in universities and mainline seminaries who interpret Genesis 1-2 as Ancient Near Eastern mythopoetic literature. They set the reading's canonical status through peer review, publication in scholarly journals, and teaching in universities and mainline seminaries. They benefit by gaining coherence between text and science, and by relieving educated believers from the burden of defending cosmological claims unsupported by evidence. Their exit options are high: they can move between interpretive traditions within scholarship, or exit to other fields.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, agenda_setter,
    organized, generational, arbitrage, global).

% Educated believers who want to maintain religious faith while accepting scientific cosmology and evolutionary biology. They adopt the allegorical reading to gain cognitive coherence and to participate fully in secular institutions (universities, medical schools, scientific professions) without constant tension between worldviews. They benefit by eliminating the need to defend young-earth chronology or reject evolutionary evidence. Their exit is constrained: leaving the reading means either abandoning science or abandoning religion.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_adherents_seeking_science_compatibility, beneficiary,
    moderate, biographical, constrained, global).

% Religious communities and denominations committed to reading Genesis 1-2 as literal historical-scientific chronicle. They bear the cost of institutional marginalization: the allegorical reading is canonical in universities and mainline seminaries, which means their interpretation is treated as less educated or less sophisticated in elite academic and professional contexts. They are partly excluded from these institutional spaces, and their members face social pressure to adopt the allegorical reading if they pursue higher education. Their identity is fused with literal reading; adopting the allegorical frame means loss of a core identity commitment.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_communities, payer,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_communities, excluded).

% Organizations built on young-earth literalism: creation museums, fundamentalist seminaries, homeschool curricula, conservative denominations. They face institutional pressure from the prevalence of the allegorical reading in mainstream scholarship and higher education. Their organizational survival depends on maintaining literalist doctrine; widespread adoption of the allegorical reading would dissolve their raison d'être. They are trapped because the constraint's dominance in elite institutions directly threatens their institutional identity.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationist_institutional_networks, payer,
    organized, generational, trapped, regional).

% Believers who treat Genesis 1-2 as theological framework compatible with science (days as epochs or literary device, not strict allegory). They gain institutional legitimacy from the allegorical reading's dominance — they can claim sophisticated hermeneutics and science compatibility. But they also pay by constantly defending the middle position against both literalists (who see them as compromising Scripture) and strict allegoricalists (who see them as over-reading theological import into cosmological framework). Their position is unstable and requires continuous rhetorical work.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary_intermediates, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary_intermediates, payer).

% Theologians and ethicists who ground creation care, environmental ethics, or human dominion frameworks in Genesis 1:28 and 2:15. The allegorical reading's decoupling of the text from factual reference also decouples the dominion mandate from normative force as a divine imperative. They lose a key scriptural warrant for specific environmental ethics; the mandate becomes a literary motif rather than a binding theological principle. They bear the cost of having to rebuild ethical frameworks without this textual foundation.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theologians_emphasizing_dominion_mandate, payer,
    moderate, biographical, constrained, global).

% Cosmologists, evolutionary biologists, geologists whose work describes Earth's actual age, evolutionary history, and cosmic origins. The allegorical reading does not constrain their work, and it removes the constraint that Genesis literalism imposes on religious adherents. They observe the hermeneutical debate and may comment on the empirical facts (e.g., confirming the evidence that contradicts young-earth chronology), but they are not parties to the interpretive constraint itself.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_scientists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the apparent contradiction between biblical authority and scientific cosmology by positioning Genesis 1-2 as theological rather than scientific discourse — believers can accept both the text's religious meaning and contemporary science's cosmological accuracy without cognitive conflict.
% TRANSFER_FUNCTION: Moves interpretive authority from the text as historical-scientific record to ancient Near Eastern literary convention and theological symbolism. Religious communities adopt this reading and gain compatibility with secular institutions; literalist communities lose adjudicative standing to defend cosmological claims from the text.
% ABSENT_VOICES: Young-earth literalists are partly excluded from elite academic and scientific spaces where the allegorical reading is canonical, though they retain institutional strongholds (fundamentalist seminaries, creation museums, some evangelical denominations). They would object that the reading abandons the text's plain sense and divine authority; their objection is structurally suppressed by the institutional dominance of critical scholarship in universities and mainstream theology.
% DISAPPEARANCE_RATIONALE: If the allegorical reading vanished — i.e., if scholars and religious institutions reverted to treating Genesis 1-2 as binding cosmological authority — the science-religion conflict would sharpen: either religious adherents would have to reject contemporary cosmology wholesale (young-earth creationism would become the only theologically coherent position for those taking the text literally) or they would have to abandon the text's authority entirely. The institutional space for religious practitioners in secular universities would contract; the young-earth creationist movement would gain relative institutional authority within religious communities.
% FOUNDING_PROBLEM: Nineteenth-century higher criticism and twentieth-century scientific cosmology made the Genesis account incompatible with accepted evidence about Earth's age, evolutionary history, and cosmic origins. Educated religious communities faced a forced choice: reject science or reject the text's authority. The allegorical reading emerged as a third option: reframe the text's genre and purpose to decouple it from cosmological claims.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary science (cosmology, evolutionary biology, geology) continues to produce evidence incompatible with young-earth and literal-creation-week readings. Critical biblical scholarship (source criticism, form criticism, comparative Ancient Near Eastern studies) continues to document the text's literary dependence on Mesopotamian creation myths (Enuma Elish, Atrahasis) and its function as theological rather than scientific literature. Both secular and religious scholarly communities outside the fundamentalist literalist movement attest that the original founding problem — the conflict between text and evidence — remains unresolved for literalist positions and is solved by the allegorical reading.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the reading solves a genuine coordination problem (science-religion conflict) but does so by shifting interpretive authority from text to scholarly consensus — a gain for those seeking science compatibility, a loss for those defending literalism. Suppression is low and declining (0.35 → 0.22 over 1880-2026) because the allegorical reading is now institutional orthodoxy in universities and mainstream theology; young-earth positions are excluded from these spaces but are not actively suppressed within their own institutional networks (creation museums, fundamentalist seminaries, homeschool curricula maintain robust literalist interpretation). Theater is low but stable (0.08 → 0.18) because the reading performs genuine hermeneutical work (reconciling text with genre conventions and Ancient Near Eastern parallels) but also performs institutional boundary-maintenance (signaling educated, cosmopolitan adherence to mainstream scholarship). The measurement series tracks the reading's institutional triumph from marginality in 1880 (when higher criticism was still contested) to near-canonical status in 2026 across universities and mainline denominations, with suppression declining as the reading becomes self-evident rather than contested.
 *
 * PERSPECTIVAL GAP:
 *   From the critical scholar's seat, the allegorical reading is interpretive clarity — seeing the text as it actually is (Ancient Near Eastern literature) rather than as it was misread (historical chronicle). From the young-earth literalist's seat, the reading is a dissolution of textual authority and a capitulation to secular pressure. From the theistic evolutionist's seat, the reading is either perfect (completely decouples text from cosmology) or inadequate (fails to preserve theological specificity about creation). The engine computes these divergences from the stakeholder structural data: scholars have high mobility and institutional power (d → beneficiary end); literalists have identity_locked exit and organized institutional power but are excluded from academic spaces (d mixed: powerful at one power level, powerless at another); theistic evolutionists are constrained and must defend a middle position continuously (d → symmetric). The reading's computed type at each seat should reflect these asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical scholars benefit from the allegorical reading by gaining scholarly coherence, institutional legitimacy, and relief from defending unsupported historical claims. They are agenda-setters within universities and mainline denominations — they set the reading's canonical status through textbooks, commentaries, seminary curricula. Religious adherents seeking science compatibility benefit by adopting the reading and gaining coherence. Young-earth communities pay by losing institutional standing and having to defend cosmological claims against consensus. Theistic evolutionists are dual-positioned: they benefit by gaining science compatibility and scholarly legitimacy, but pay by having to defend the middle position against both literalist and strict-allegorical critiques. Dominion-mandate theologians pay by losing normative force for a key theological principle. No directionality_overrides are needed because the structural derivation from beneficiary/victim declarations and exit_options produces accurate d values: scholars and science-seeking believers are near the beneficiary end (low d); literalists are near the target end (high d); theistic evolutionists are constrained and symmetric (d ≈ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (science-text conflict) is live: contemporary cosmology and evolutionary biology continue to produce evidence incompatible with young-earth and literal-creation-week readings. The allegorical reading's success has not resolved the founding problem for those committed to literalism; it has instead created two populations with incompatible commitments: those who adopt the reading and gain science compatibility, and those who reject it and maintain literal cosmological claims. The reading has not made the founding problem disappear; it has institutionalized one solution and marginalized alternatives. This is not mandatrophy (a constraint persisting after its founding problem dies) but rather institutionalization of a live sectional divide. The reading solves the problem for one population (science-seeking believers) and intensifies it for another (literalists who must now defend their position against institutional consensus). No mandatrophy declaration is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_near_eastern_dependency_thesis,
    'Is the Genesis creation account dependent on Mesopotamian source materials (Enuma Elish, Atrahasis, other ANE myths), or does it represent an independent theological statement that happens to share literary conventions with ANE literature?',
    'Comparative literary analysis establishing direct textual borrowing versus convergent use of shared cultural motifs. Requires specification of what counts as dependency (verbal parallels, narrative structure, mythological elements, theological opposition) versus shared literary environment.',
    'If dependency is established, the allegorical reading is strengthened (Genesis is Ancient Near Eastern literature using ANE conventions). If independence is established, the reading remains valid but the genre assignment becomes more ambiguous — the text could be original theological work using shared conventions rather than literature-within-the-tradition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ancient_near_eastern_dependency_thesis, empirical, 'The degree of Genesis''s literary dependence on Mesopotamian creation myths').

omega_variable(
    theological_meaning_without_cosmological_reference,
    'Can the theological meaning (God''s sovereignty, humanity''s moral standing, the goodness of creation) be maintained if the cosmological claims are completely false? Or does false cosmology undermine theological truth?',
    'Theological argument from within the allegorical reading tradition: if the meaning is genuinely independent of accuracy, then a text that teaches good theology but false cosmology is internally coherent; if the meaning depends on referential truth, then false cosmology corrupts the theological content.',
    'If theological meaning is independent of cosmological accuracy, the allegorical reading is fully coherent and the founding problem is completely solved for those who adopt it. If meaning requires referential truth, the reading faces a residual coherence problem: the text cannot be both false about cosmology and true about theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_meaning_without_cosmological_reference, conceptual, 'Whether theological truth is independent of cosmological accuracy in the Genesis account').

omega_variable(
    institutional_dominance_versus_truth,
    'Is the allegorical reading gaining canonical status because it is true and defensible on its merits, or because it serves the institutional interests of secular universities and mainline denominations that seek to remove conflict with science?',
    'Examine the reading''s scholarly arguments independent of institutional context. If the reading''s hermeneutical case is sound (textual analysis, ANE parallels, genre identification), institutional dominance reflects accurate judgment. If the case is weak and dominance reflects institutional pressure, the reading is extractive (imposing a particular interpretation through power rather than persuasion).',
    'If the reading is institutionally dominant because it is true, the extractiveness should decline as the reading becomes self-evident and uncontested. If dominance reflects institutional power rather than persuasive force, extractiveness will increase as the reading consolidates — the measurement series shows extractiveness increasing slightly (0.15 → 0.38) and suppression declining sharply (0.35 → 0.22), which suggests institutional consolidation rather than growing persuasive consensus among literalists. This warrants close examination of whether the reading''s dominance reflects genuine scholarly persuasion or institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dominance_versus_truth, preference, 'Whether the reading''s institutional dominance reflects scholarly truth or institutional power').

omega_variable(
    dominion_mandate_normative_weight,
    'If Genesis 1:28 and 2:15 are read allegorically as theological statements rather than factual mandates, what normative force do they retain for human environmental behavior and resource use?',
    'Theological argument about whether allegorical reading of the dominion mandate preserves or eliminates its ethical implications. Can an allegorical reading produce binding ethical claims, or does decoupling from factual reference necessarily eliminate prescriptive force?',
    'If the allegorical reading can preserve normative force for the dominion mandate, the constraint''s cost for dominion-focused theologians is reduced. If the reading necessarily eliminates prescriptive force, dominion-focused communities face a genuine loss: the scriptural warrant for specific environmental ethics or creation-care frameworks is undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_mandate_normative_weight, conceptual, 'The normative weight of the dominion mandate under allegorical reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1880, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1880, 0.08).
narrative_ontology:measurement_basis(gene_tr_t1880, observed).
narrative_ontology:measurement(gene_tr_t1920, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1920, 0.11).
narrative_ontology:measurement_basis(gene_tr_t1920, observed).
narrative_ontology:measurement(gene_tr_t1960, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1960, 0.13).
narrative_ontology:measurement_basis(gene_tr_t1960, observed).
narrative_ontology:measurement(gene_tr_t1990, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(gene_tr_t1990, observed).
narrative_ontology:measurement(gene_tr_t2010, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2010, 0.17).
narrative_ontology:measurement_basis(gene_tr_t2010, observed).
narrative_ontology:measurement(gene_tr_t2026, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(gene_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1880, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement_basis(gene_be_t1880, observed).
narrative_ontology:measurement(gene_be_t1920, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement_basis(gene_be_t1920, observed).
narrative_ontology:measurement(gene_be_t1960, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement_basis(gene_be_t1960, observed).
narrative_ontology:measurement(gene_be_t1990, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement_basis(gene_be_t1990, observed).
narrative_ontology:measurement(gene_be_t2010, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement_basis(gene_be_t2010, observed).
narrative_ontology:measurement(gene_be_t2026, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(gene_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1880, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement_basis(gene_su_t1880, observed).
narrative_ontology:measurement(gene_su_t1920, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement_basis(gene_su_t1920, observed).
narrative_ontology:measurement(gene_su_t1960, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1960, 0.24).
narrative_ontology:measurement_basis(gene_su_t1960, observed).
narrative_ontology:measurement(gene_su_t1990, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement_basis(gene_su_t1990, observed).
narrative_ontology:measurement(gene_su_t2010, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2010, 0.22).
narrative_ontology:measurement_basis(gene_su_t2010, observed).
narrative_ontology:measurement(gene_su_t2026, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(gene_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.12).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The 'genesis_creation_narrative' kernel decomposes into three structurally distinct constraints, each instantiating a different reading: allegorical_ancient_near_east (text has no cosmological authority; theological meaning decoupled from accuracy), literal_young_earth (text is inerrant cosmological chronicle; young-earth timescale and six literal days), and theistic_evolutionary (text is theological framework compatible with science; days as epochs or literary device). The ε values differ sharply because the readings have different victim/beneficiary structures and different relationships to scientific consensus. This story instantiates the allegorical reading; the sibling stories instantiate the other readings. All three are linked via network.affects_constraints because they compete for interpretive authority over the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
