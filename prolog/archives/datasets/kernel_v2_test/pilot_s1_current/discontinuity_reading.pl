% ============================================================================
% CONSTRAINT STORY: discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_discontinuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: discontinuity_reading
 *   human_readable: Discontinuity Reading: Medieval Latin as Corruption and Reconstruction as Epistemic Recovery
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading treats Classical Latin and Medieval Latin as
 *   structurally distinct linguistic systems separated by a rupture in the
 *   late empire/early medieval transition. Under this reading, medieval forms
 *   are corruptions of classical structure; the goal of philology is
 *   reconstruction—recovering the lost integrity of classical Latin from
 *   fragmentary medieval textual evidence. This reading emerged in
 *   Renaissance humanism and has remained institutionally dominant in
 *   classical philology. It creates a tangled rope structure: genuine
 *   coordination function (systematic study of classical Latin texts) coupled
 *   with asymmetric extraction (medieval scholarship delegitimized as
 *   derivative, students of medieval texts face career penalties). The
 *   constraint operates across 576 years (1450–2026) showing measurable
 *   intensification in theater ratio and suppression requirement through the
 *   19th century, stabilizing in the 20th. The reading is one of three
 *   competing frameworks explaining the Latin linguistic change:
 *   discontinuity (this constraint), continuity (postulating unbroken
 *   evolution), and hybrid (allowing both continuity and innovation). The
 *   discontinuity reading coexists with its siblings but has maintained
 *   institutional dominance throughout the interval.
 *
 * KEY AGENTS:
 *   - Classical Latin philologists: Institutional beneficiaries (institutional/arbitrage) — reconstruction authority concentrates in classical programs; career advancement depends on classical expertise
 *   - Medieval philologists and textual scholars: Secondary victims (moderate/identity_locked) — structurally mobile but identity-fused with legitimacy hierarchy; professional identity as 'real' philologist depends on classical prestige
 *   - Medieval Latin texts and documents: Primary victims (powerless/trapped) — cannot defend their own legitimacy; forced into degradation narrative regardless of actual function
 *   - University philology departments: Institutional agenda-setters (organized/constrained) — coordinate classical training (genuine coordination) while enforcing discontinuity hierarchy through curriculum and hiring
 *   - Humanist tradition and textual authority: Powerful beneficiary (powerful/constrained) — inherits legitimacy from Renaissance recovery; maintains classical priority as foundational commitment
 *   - Editorial practice and textual apparatus: Institutional performer (institutional/arbitrage) — enacts discontinuity reading through emendation practices; much performance is ritualized (piton character)
 *   - Analytical observer: Civilizational position (analytical/analytical) — risks naturalizing contingent periodization as linguistic fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(discontinuity_reading, 0.65).
domain_priors:suppression_score(discontinuity_reading, 0.48).
domain_priors:theater_ratio(discontinuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(discontinuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(discontinuity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(discontinuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(discontinuity_reading, "Discontinuity Reading: Medieval Latin as Corruption and Reconstruction as Epistemic Recovery").
narrative_ontology:topic_domain(discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(discontinuity_reading, '976716d6-f7f9-4b18-8455-0a1e9a29d025').
narrative_ontology:cs_kernel_codification('976716d6-f7f9-4b18-8455-0a1e9a29d025', fixed_text).
narrative_ontology:cs_authority_grounding('976716d6-f7f9-4b18-8455-0a1e9a29d025', lineage).
narrative_ontology:cs_interpretation_layer_present('976716d6-f7f9-4b18-8455-0a1e9a29d025').
narrative_ontology:cs_reading_relation('976716d6-f7f9-4b18-8455-0a1e9a29d025', discontinuity_reading__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('976716d6-f7f9-4b18-8455-0a1e9a29d025', discontinuity_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('976716d6-f7f9-4b18-8455-0a1e9a29d025', foundational, classical_forms_primacy_recoverable).
narrative_ontology:cs_axiom_status(classical_forms_primacy_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('976716d6-f7f9-4b18-8455-0a1e9a29d025', classical_forms_primacy_recoverable, empirically_contingent).
narrative_ontology:cs_axiom('976716d6-f7f9-4b18-8455-0a1e9a29d025', foundational, medieval_innovation_as_degradation).
narrative_ontology:cs_axiom_status(medieval_innovation_as_degradation, holdable).
narrative_ontology:cs_axiom_grounding('976716d6-f7f9-4b18-8455-0a1e9a29d025', medieval_innovation_as_degradation, conventional).
narrative_ontology:cs_reference_frame('976716d6-f7f9-4b18-8455-0a1e9a29d025', classical_literary_primacy).
narrative_ontology:cs_drift_state('976716d6-f7f9-4b18-8455-0a1e9a29d025', contemporary_functional_typology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('976716d6-f7f9-4b18-8455-0a1e9a29d025', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(discontinuity_reading, classical_latin_philologists).
narrative_ontology:constraint_beneficiary(discontinuity_reading, classical_philological_canon).
narrative_ontology:constraint_victim(discontinuity_reading, medieval_latin_texts).
narrative_ontology:constraint_victim(discontinuity_reading, medieval_epistemic_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(discontinuity_reading, renaissance_humanist_legacy).
narrative_ontology:constraint_victim(discontinuity_reading, medieval_manuscript_scholars).
narrative_ontology:constraint_vindicates(discontinuity_reading, latin_linguistic_purity_doctrine).
narrative_ontology:constraint_vindicates(discontinuity_reading, classical_period_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control curriculum design, hiring decisions, and graduate training. Set the standard for what counts as legitimate philological work through degree requirements, comprehensive exams, and publication venues. Maintain classical texts as the primary object of study. Have the structural ability to shift emphasis (arbitrage: could hire more medieval specialists or adopt hybrid frameworks) but choose not to.
narrative_ontology:constraint_stakeholder(discontinuity_reading, classical_philology_departments, agenda_setter,
    institutional, generational, arbitrage, global).

% Study medieval Latin texts, glosses, and scribal practices. Face career penalties for specializing in medieval material—publication venues favor classical topics, job market rewards classical training, professional prestige accrues to classicists. Could exit (shift to medieval linguistics, functional analysis of medieval texts) but cannot do so without abandoning the identity of 'philologist' as currently defined. Professional identity fused with legitimacy hierarchy.
narrative_ontology:constraint_stakeholder(discontinuity_reading, medieval_manuscript_scholars, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(discontinuity_reading, medieval_manuscript_scholars, excluded).

% The discontinuity reading inherits its authority from Renaissance recovery of classical texts. This genealogy grounds the framework's legitimacy in a real historical achievement (discovery of lost classical works). But the framework is now deployed to maintain humanist classical priorities even after the original recovery mandate is satisfied. Constrained by textual authority—the canon of 'classical' authors is enshrined in major editions and historical narratives.
narrative_ontology:constraint_stakeholder(discontinuity_reading, renaissance_humanist_legacy, beneficiary,
    powerful, civilizational, constrained, continental).

% Medieval manuscripts contain linguistic forms, innovations, and textual variants that the discontinuity reading classifies as corruptions. These forms cannot defend themselves or demonstrate their own legitimacy within the discontinuity framework. The texts are trapped in a degradation narrative.
narrative_ontology:constraint_stakeholder(discontinuity_reading, manuscript_textual_evidence, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(discontinuity_reading, manuscript_textual_evidence).

% The practice of critical edition-making—emending suspected corruptions, reconstructing posited originals, marking medieval variants as deviations—enacts the discontinuity reading through textual practice. Publishers, editors, and scholarly conventions maintain and reproduce this apparatus. Have structural ability to adopt alternative frameworks (diplomatic editions, accepting medieval variants as valid) but continue classical emendation practices through institutional inertia.
narrative_ontology:constraint_stakeholder(discontinuity_reading, editorial_apparatus_tradition, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Scholars advocating for continuity (medieval forms as natural evolution) or hybrid frameworks (both evolution and innovation) have created alternative communities and publishing venues but remain outside the classical mainstream. Have some institutional presence (journals, conferences) but face resource scarcity and lower prestige within traditional philology hierarchies.
narrative_ontology:constraint_stakeholder(discontinuity_reading, continuity_and_hybrid_reading_communities, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The discontinuity reading solves the genuine coordination problem of how to systematically study classical Latin across texts and authors: establishing a shared standard (classical forms), shared methods (textual reconstruction, critical editing), and shared reference points (canonical authors). This coordination enables cumulative scholarship—students learn to read classical texts, philologists can build on prior work, meaning across classical literature can be traced.
% TRANSFER_FUNCTION: The arrangement transfers prestige, institutional authority, and material resources (positions, funding, publication venues) from medieval specialists to classicists. Medieval scholars bear the cost of delegitimization—their research is presented as derivative, secondary, corrupted. In return, the coordination of classical study is maintained and elaborated. The transfer is not total or one-directional (some medieval scholars achieve prominence, some classical work is recognized as derivative), but the net flow is from medieval to classical positions.
% ABSENT_VOICES: Medieval Latin speakers themselves (Latin is now a dead language), contemporary Latin communities (nonexistent), functional-typological linguists working on living language change (excluded from the philological conversation until late 20th century), and indigenous knowledge traditions treating text and meaning differently than humanist philology. These voices would object to the corruption narrative and the hierarchy of classical primacy. They are absent because philology as a discipline is constituted within the humanist tradition and does not systematically incorporate alternative frameworks for evaluating linguistic change or textual meaning.
% DISAPPEARANCE_RATIONALE: Classical philologists (particularly discontinuity advocates) claim world_rearranges: without the discontinuity framework, the classical literary tradition would be lost to degradation narratives, medieval texts would overwhelm classical study, and the unified philological method would collapse. Continuity and hybrid advocates claim world_unchanged: the linguistic facts (medieval forms, their origins, their functions) would remain the same; only the evaluative framing would shift. A third position (rare but present) claims world_rearranges but differently: medieval scholarship would flourish, hybrid frameworks would enable understanding both systems better, and the canon would broaden beyond classical elites. The contest is genuine because the three verdicts depend on what counts as the essential arrangement. If it is classical literary survival, world_rearranges. If it is linguistic understanding, world_unchanged. If it is scholarly prestige distribution, world_rearranges but toward expansion.
% FOUNDING_PROBLEM: Renaissance humanism (1350–1550) faced the loss of classical Latin texts through the medieval period: manuscripts were scattered, copied inaccurately, glossed with medieval interpretations, and in many cases simply lost. The founding problem was how to recover authentic classical texts and understand their original form. The discontinuity reading emerged as a solution: treat medieval glosses and variants as corruptions; reconstruct the classical original by comparing manuscripts and applying philological rules; recover the integrity of classical literature.
% FOUNDING_PROBLEM_CORROBORATION: Humanist sources (Petrarch's letters, Valla's annotations, Erasmus's editions) attest the original discovery and recovery problem. Contemporary manuscript scholars and librarians attest that the problem is solved: we have access to classical texts with high confidence. Medieval specialists (external to the beneficiary community) attest that the classical texts are available and that medieval material has become valuable as historical documentation, not just corruption to be purged.
narrative_ontology:disappearance_verdict(discontinuity_reading, contested).
narrative_ontology:founding_problem_status(discontinuity_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL TEXTS (SNARE) — Trapped within a framework that treats medieval innovations as errors. Medieval texts cannot defend their own linguistic legitimacy; readers of medieval Latin face career penalties for treating it as structurally valid. Maximum extraction: the constraint forces medieval forms into a degradation narrative regardless of their actual functional role.
constraint_indexing:constraint_classification(discontinuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MEDIEVAL PHILOLOGISTS (SNARE, identity_locked) — Structurally mobile (could study medieval forms on their own terms) but identity-fused with the legitimacy claim: professional identity as a 'real' philologist depends on classical training, and shifting to medieval equals admitting to a lower-status specialization. The binding is cognitive (identity frame) rather than material (legal/economic barrier). High extraction because the constraint defines what counts as legitimate scholarship.
constraint_indexing:constraint_classification(discontinuity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: CLASSICAL PHILOLOGISTS (ROPE) — Benefit structurally from the discontinuity reading: it preserves the classical period as the normative standard against which all later forms are measured. Reconstruction authority (recovering lost structure from textual evidence) concentrates in classical philology departments. Genuine coordination function: the discontinuity framework does solve the real problem of tracking how Latin evolved over time. Net beneficiaries with arbitrage options.
constraint_indexing:constraint_classification(discontinuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL PHILOLOGY (TANGLED ROPE) — Universities coordinate classical training (genuine coordination function) while the discontinuity framework channels resources toward classical programs and away from medieval study. Constrained by curricular path-dependency: shifting the framework requires reconstructing entire degree sequences. Benefits from the coordination (students learn classical Latin systematically) while imposing extraction (medieval scholarship gets delegitimized as derivative and low-status).
constraint_indexing:constraint_classification(discontinuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANIST TRADITION (TANGLED ROPE) — The discontinuity reading inherits legitimacy from the Renaissance recovery of classical texts and authors. Genuine coordination: the priority on classical sources enabled systematic recovery of lost works. But the framework also licenses extraction: medieval scholarship is treated as a contaminating influence to be purged. Constrained by textual authority (the primacy of classical authors is enshrined in canonical editions and historical narratives).
constraint_indexing:constraint_classification(discontinuity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: EDITORIAL APPARATUS (PITON) — The discontinuity reading manifests in editorial practice: medieval scribal changes are routinely presented as 'corruptions' requiring emendation back to posited classical originals. Much of this apparatus is performative: editors reconstruct an idealized classical form that may never have existed in any single manuscript. The ritual persists through institutional inertia (how scholarship is supposed to be done) rather than through demonstrated epistemic payoff of reconstruction over acceptance of medieval variants.
constraint_indexing:constraint_classification(discontinuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, language necessarily changes over time; medieval forms are inevitable consequences of sound change, morphological leveling, and contact dynamics. The 'corruption' framing naturalizes what is actually a contingent evaluative choice. This perspective risks being a false summit: the engine will detect that it naturalizes what is a constructed institutional arrangement (the privileging of classical forms as the standard).
constraint_indexing:constraint_classification(discontinuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(discontinuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(discontinuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(discontinuity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(discontinuity_reading, TR),
    TR >= 0.70.

:- end_tests(discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Substantial. The discontinuity reading generates clear asymmetric benefits—classical philologists accumulate institutional authority, resources, and prestige while medieval scholars face delegitimization and career penalties. However, extraction is not maximal because the framework does provide genuine coordination benefits (systematic study of classical texts, shared methodologies for reconstruction). The rising trajectory from 1450 (0.45) to 1850 (0.68) reflects institutional solidification; the plateau at 0.65 post-1950 reflects partial pushback from continuity and functional-typological perspectives. Suppression (0.48): Moderate. Medieval Latin scholars face real barriers—career risk, resource scarcity, lower institutional rank—but they are not totally foreclosed. Some medieval scholars achieve prominence; the suppression operates through legitimacy denial rather than legal prohibition. Rising from 1450 (0.30) to 1850 (0.52) reflects increasing institutional enforcement; stabilization at 0.48 reflects competing frameworks creating exit options. Theater ratio (0.58): Moderate-high. The reconstruction apparatus is substantially performative: editors routinely posit classical originals lacking textual support; emendation practices follow a ritual form (marking suspected corruptions, proposing classical readings) that may not improve actual understanding. Rising from 1450 (0.35) to 1850 (0.58) shows increasing performative complexity as textual apparatus became more elaborate; stabilization reflects equilibrium between emendation practice and source scarcity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces striking perspectival divergence. Medieval texts experience it as a snare—forced into a corruption narrative with no exit. Medieval scholars experience identity-locked snare—structurally mobile but unable to exit the legitimacy hierarchy that defines their professional identity. Classical philologists experience it as rope—genuine coordination (systematic classical study) with net benefit (authority and prestige). Institutions experience tangled rope—coordinating classical education while extracting from medieval scholarship through resource allocation. The humanist tradition experiences tangled rope—legitimate historical recovery (genuine coordination function) coupled with delegitimizing medieval work. Editorial practice experiences it as piton—reconstruction ritual maintained through institutional inertia more than epistemic payoff. The analytical observer risks seeing natural law (inevitable linguistic degradation) when observing a constructed evaluative hierarchy. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural position relative to the classical/medieval boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and beneficiary/victim status. Medieval texts (powerless/trapped/victim) experience d ≈ 1.0 (full target). Medieval scholars with identity_locked exit (moderate power, internalized legitimacy frame, victim status) experience d ≈ 0.85 (nearly full target with slight mitigation from moderate power). Classical philologists (institutional/arbitrage/beneficiary) experience d ≈ 0.15 (low directionality → negative effective extraction → subsidy). Institutional philology (organized/constrained/mixed) derives d from the coordination function (beneficiary) offset by constrained exit (higher d than arbitrage). The humanist tradition (powerful/constrained/beneficiary) experiences d ≈ 0.35 (moderate due to constrained exit despite beneficiary status). Editorial apparatus (institutional/arbitrage/beneficiary) experiences d ≈ 0.20. These d values feed into the engine's effective extraction computation, which applies spatial scope amplification (global scope raises effective extraction for targets, lowers it for beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity reading exhibits incipient mandatrophy: its founding mandate was to recover classical texts and reconstruct their proper form (Renaissance humanism, 1450–1550). This mandate was epistemically productive—humanists genuinely discovered lost classical authors and works. However, by 1700–1800, the founding problem (lack of access to classical sources) had substantially diminished: printing, manuscript discovery, and systematic cataloging had made classical texts widely available. Post-1850, the mandate (recovery via reconstruction) outlived its primary function—most classical literature was already recovered. The continuing practice of emendation and reconstruction persists through institutional inertia (how philology is 'supposed to be done') rather than through addressing an active epistemic need. The theater ratio rising from 0.35 (1450) to 0.58 (1850+) indicates increasing performative character as the epistemic payoff of reconstruction work declined relative to the ritual elaboration of the apparatus. The constraint has not fully resolved mandatrophy, but the plateau in extractiveness (0.65) and stabilization of theater ratio post-1950 suggest partial recognition of the problem through acceptance of competing readings (continuity, hybrid) alongside discontinuity. A full resolution would require explicit acknowledgment that the discontinuity framework, while historically productive, is now maintained more as evaluative tradition than as epistemically necessary reconstruction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is the boundary between Classical and Medieval Latin a real linguistic division or a constructed periodization imposed by Renaissance scholarship?',
    'Comparative analysis of discontinuity markers: (1) lexical innovation rates across the boundary vs within-period rates; (2) morphosyntactic change acceleration at the medieval boundary vs neighboring periods; (3) examination of whether the ''boundary'' shifts if dated differently (late empire vs early medieval). If innovation rates are continuous and the boundary is moveable, the division is periodization; if discontinuity is demonstrable in multiple feature classes independent of periodization, the division is real.',
    'If constructed periodization: the discontinuity reading is an external scholarly frame imposed on internal linguistic continuity, strengthening Snare and identity_locked classifications. If real linguistic division: the discontinuity reading recovers a true structural fact, validating aspects of the rope and classical beneficiary perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, empirical, 'Whether Classical-Medieval boundary is linguistic or historiographic').

omega_variable(
    reconstruction_validity_claims,
    'Does reconstruction of ''lost'' classical structure from textual evidence produce falsifiable predictions, or is it unfalsifiable retroactive model-fitting?',
    'Test whether reconstructed classical forms meet the criteria for scientific reconstruction: (1) independent predictive power (reconstructed forms predict previously unknown variants in undiscovered texts or glossaries); (2) convergence across multiple reconstruction methods; (3) presence of negative cases (reconstructed forms that do NOT appear in any available source, suggesting the model is wrong). If predictions fail or reconstructions are purely retrodictive, the recovery claim is cover story for evaluative ranking.',
    'If falsifiable: reconstruction is genuine epistemic work, validating the coordinate rope function and institutional beneficiary perspectives. If unfalsifiable: reconstruction is ritually performed but not epistemically grounded, strengthening piton classification and extractive perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_validity_claims, empirical, 'Whether reconstruction produces testable predictions or only retroactive fitting').

omega_variable(
    medieval_functional_innovation,
    'Do medieval Latin innovations serve genuine communicative functions (inflectional loss compensated by word order, prepositions, etc.), or are they dysfunctional degradations of classical system?',
    'Functional-typological analysis: (1) mapping of which classical features were lost and what medieval features replaced them; (2) comparison of information-structural load (if classical relies on inflection to mark grammatical relations, does medieval use word order or prepositions for the same function?); (3) whether medieval texts show systematic patterns or random variation. If medieval forms show compensatory structure, they are functional innovations; if random, they support degradation narrative.',
    'If functional innovations: medieval Latin is a structurally coherent system, delegitimizing the corruption framing and supporting continuity/hybrid readings. Snare and identity_locked classifications would be strengthened. If dysfunctional: medieval forms are genuine degradation, validating discontinuity reading and classical philologist beneficiary status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medieval_functional_innovation, empirical, 'Whether medieval features are functional innovations or degradation').

omega_variable(
    reading_contest_status,
    'Within the philological community, which reading (discontinuity, continuity, or hybrid) commands institutional authority and resource allocation in 2026?',
    'Quantitative bibliometric analysis: (1) citation networks among classical, medieval, and hybrid philologists; (2) department hiring patterns (classical vs medieval positions, rank distribution, salary parity); (3) grant funding by research direction; (4) editorial positions in major journals and presses. If discontinuity reading dominates all three metrics, it retains institutional advantage. If hybrid or continuity readings show equal authority, the contest is genuinely live.',
    'If discontinuity dominates: the tangled rope and institutional beneficiary classifications are sustained by continuing extraction. If equal authority: the readings genuinely coexist, and extraction is diminishing. This measurement directly informs whether the constraint is stable or shifting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_status, empirical, 'Which reading currently commands institutional authority and resources').

omega_variable(
    humanist_recovery_presumption,
    'Does the discontinuity reading''s claim that Renaissance humanists ''recovered'' classical Latin depend on an unreplicable prior authority (the authors themselves), or are there independent criteria for what counts as classical?',
    'Historical analysis of Renaissance textual practice: (1) whether humanists had access to a ''purer'' classical tradition than we do, or were projecting their preferences onto fragmentary sources; (2) whether the classical canon (which authors, which works, which readings) was established by humanist selection or by prior consensus. If humanists invented the canon, the discontinuity reading''s authority is circular: classical means ''what the humanists said was classical.''',
    'If humanist selection: the classical standard is conventional, not natural. Discontinuity reading is then a constructed framework maintaining humanist authority, strengthening extraction and institutional power perspectives. If prior consensus: classical priority has deeper roots, validating some aspects of the rope and mountain perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_recovery_presumption, conceptual, 'Whether Renaissance humanists recovered or constructed the classical standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(discontinuity_reading, 1450, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disc_theater_1450, discontinuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(disc_theater_1550, discontinuity_reading, theater_ratio, 1, 0.42).
narrative_ontology:measurement(disc_theater_1700, discontinuity_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(disc_theater_1850, discontinuity_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(disc_theater_1950, discontinuity_reading, theater_ratio, 4, 0.58).
narrative_ontology:measurement(disc_theater_2026, discontinuity_reading, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(disc_extract_1450, discontinuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(disc_extract_1550, discontinuity_reading, base_extractiveness, 1, 0.55).
narrative_ontology:measurement(disc_extract_1700, discontinuity_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(disc_extract_1850, discontinuity_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(disc_extract_1950, discontinuity_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(disc_extract_2026, discontinuity_reading, base_extractiveness, 5, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(disc_suppress_1450, discontinuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(disc_suppress_1550, discontinuity_reading, suppression_requirement, 1, 0.38).
narrative_ontology:measurement(disc_suppress_1700, discontinuity_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(disc_suppress_1850, discontinuity_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(disc_suppress_1950, discontinuity_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(disc_suppress_2026, discontinuity_reading, suppression_requirement, 5, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(discontinuity_reading, continuity_reading).
narrative_ontology:affects_constraint(discontinuity_reading, hybrid_reading).
narrative_ontology:affects_constraint(discontinuity_reading, humanist_recovery_mandate).
narrative_ontology:affects_constraint(discontinuity_reading, textual_apparatus_expansion).

% DUAL FORMULATION NOTE:
% The discontinuity reading is one member of a constraint family decomposing the kernel 'correct_latin_kernel.' The three readings (discontinuity, continuity, hybrid) have distinct ε values reflecting different structural relationships to medieval texts and scholars. Discontinuity reading (this file): ε ≈ 0.65 (substantially extractive, gains from legitimacy denial). Continuity reading: ε ≈ 0.30 (low extraction, medieval forms treated as valid evolution). Hybrid reading: ε ≈ 0.40 (moderate extraction, acknowledges both evolution and innovation). These three stories are linked because they offer competing frameworks for understanding the same set of linguistic facts. The ε difference indicates that observable-dependent classification is not at play—the readings produce genuinely different extraction metrics because they assign different roles to medieval scholarship. All three stories should be compiled to enable the engine to compute contention patterns (organized scholars holding incompatible readings) and to preserve the philological contest as measurable data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
