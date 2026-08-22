% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Rupture Reading of Classical Latin Correctness (Renaissance Humanist Standard)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The rupture reading treats Classical Latin as a fixed,
 *   textually-recoverable standard whose authority derives exclusively from
 *   ancient sources (chiefly Cicero and the Augustan canon), and treats the
 *   entire medieval Latin tradition — scholastic philosophy, canon and civil
 *   law, technical and notarial registers — as corruption to be corrected
 *   rather than a legitimate stage of linguistic evolution. This is
 *   Renaissance humanism's own self-understanding of its philological
 *   project: not a continuation of medieval learning but a recovery of
 *   something medieval usage had degraded. The extraction is structural: the
 *   standard was reconstructed centuries after the fact by scholars who then
 *   used it to devalue the accumulated intellectual and technical output of
 *   the preceding millennium, redirecting prestige and patronage toward those
 *   who could perform the reconstructed register.
 *
 * KEY AGENTS:
 *   - humanist_philologists: agenda_setter, institutional power, arbitrage exit — construct and police the standard
 *   - ciceronian_stylists: beneficiary, powerful, mobile exit — profit from scarce classical fluency
 *   - renaissance_printing_academies: beneficiary, organized, arbitrage exit — commercialize the standard via textbooks
 *   - medieval_scholastic_writers: payer, moderate power, trapped exit — retroactively delegitimized, cannot respond
 *   - vernacular_adjacent_technical_scribes: payer, powerless, trapped exit — classical purity structurally unattainable for their domain
 *   - canon_and_civil_law_notaries: payer, moderate power, constrained exit — precision register reframed as corruption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.71).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Rupture Reading of Classical Latin Correctness (Renaissance Humanist Standard)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '31b49557-3a9a-492d-b7da-04a8faeae24b').
narrative_ontology:cs_kernel_codification('31b49557-3a9a-492d-b7da-04a8faeae24b', fixed_text).
narrative_ontology:cs_authority_grounding('31b49557-3a9a-492d-b7da-04a8faeae24b', lineage).
narrative_ontology:cs_interpretation_layer_present('31b49557-3a9a-492d-b7da-04a8faeae24b').
narrative_ontology:cs_reading_relation('31b49557-3a9a-492d-b7da-04a8faeae24b', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('31b49557-3a9a-492d-b7da-04a8faeae24b', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('31b49557-3a9a-492d-b7da-04a8faeae24b', foundational, classical_text_is_sole_legitimate_norm).
narrative_ontology:cs_axiom_status(classical_text_is_sole_legitimate_norm, holdable).
narrative_ontology:cs_axiom_grounding('31b49557-3a9a-492d-b7da-04a8faeae24b', classical_text_is_sole_legitimate_norm, conventional).
narrative_ontology:cs_axiom('31b49557-3a9a-492d-b7da-04a8faeae24b', foundational, medieval_usage_constitutes_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_usage_constitutes_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('31b49557-3a9a-492d-b7da-04a8faeae24b', medieval_usage_constitutes_corruption_not_evolution, conventional).
narrative_ontology:cs_reference_frame('31b49557-3a9a-492d-b7da-04a8faeae24b', augustan_ciceronian_textual_canon).
narrative_ontology:cs_drift_state('31b49557-3a9a-492d-b7da-04a8faeae24b', high_renaissance_academy_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('31b49557-3a9a-492d-b7da-04a8faeae24b', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, ciceronian_stylists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, renaissance_printing_academies).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholastic_writers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_adjacent_technical_scribes).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, canon_and_civil_law_notaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and police the reconstructed classical norm (Ciceronian syntax, restored orthography, purged vocabulary) as the sole legitimate Latin. Control the philological apparatus — manuscript collation, grammar treatises, textbook production — that decides what counts as correct. Their professional standing and patronage income depend on being the arbiters of this restored standard.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, humanist_philologists, beneficiary).

% Court secretaries, diplomats, and literary figures who have invested years mastering the reconstructed classical register. Their careers, patronage, and social capital are built on demonstrating fluency in the humanist standard; the rupture framing converts their acquired skill into scarce, high-status capital by delegitimizing rival forms of Latin competence.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ciceronian_stylists, beneficiary,
    powerful, biographical, mobile, continental).

% Publish grammars, editions, and pedagogical texts certifying the classical standard. Profit directly from selling the reconstructed norm as a teachable, sellable commodity — new textbooks, new curricula, new certifying examinations — each cycle reinforcing the standard's authority and their market position.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, renaissance_printing_academies, beneficiary,
    organized, generational, arbitrage, continental).

% Centuries of accumulated scholastic Latin — Aquinas, canonists, university disputational prose — is retroactively branded 'barbarous' and corrupt. Writers trained in this register cannot simply relearn; their entire intellectual output is devalued by a standard invented after the fact and applied backward. They have no forum in which the rupture framing is contestable on its own terms.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholastic_writers, payer,
    moderate, biographical, trapped, continental).

% Notaries, apothecaries, surveyors, and craft-guild record keepers who use a practical Latin adapted to technical vocabulary absent from classical sources (no classical word for a compound interest clause or a surgical instrument). Classical purity is structurally unattainable for their domain — the standard was never built to accommodate their needs — yet their usage is now read as ignorance rather than adaptation.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_adjacent_technical_scribes, payer,
    powerless, biographical, trapped, regional).

% Draft legal instruments in a technical Latin refined over centuries for precision in contracts and canon law. The rupture standard treats this precision-oriented register as corrupted vulgar Latin, pressuring legal institutions to either adopt stylistically 'purer' but functionally poorer forms or defend their register against accusations of illiteracy.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, canon_and_civil_law_notaries, payer,
    moderate, generational, constrained, national).

% The corpus of surviving ancient texts itself — fragmentary, unevenly transmitted, filtered through medieval copyists whose own scribal choices the rupture reading must partly trust and partly discard. Named here only to note that the reconstruction depends on the very medieval transmission chain it delegitimizes.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, surviving_classical_manuscript_tradition, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(latin_correctness__rupture_reading, surviving_classical_manuscript_tradition).

% Historical linguists who would later document Latin as a continuously evolving language with no privileged synchronic cut-point are not part of the humanist-era conversation; their evidence-based continuity account arrives centuries after the rupture standard has already reorganized education, law, and letters around itself.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, later_comparative_philologists, excluded,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable, textually-anchored standard for Latin composition, enabling humanist scholars across fragmented polities to communicate, cite, and evaluate one another's Latin against a shared, verifiable model rather than an unbounded set of regional and period usages.
% TRANSFER_FUNCTION: Moves prestige, patronage income, teaching posts, and interpretive authority from writers and institutions using scholastic/technical/notarial Latin toward humanist philologists, Ciceronian stylists, and the print academies that certify the reconstructed standard — by redefining centuries of the prior register as error rather than variation.
% ABSENT_VOICES: Medieval scholastic authors (already dead, unable to respond to retroactive delegitimization), technical scribes and notaries (rarely literate in humanist polemic and without access to the philological forums where the standard is debated), and — anachronistically — the later comparative-linguistic tradition that would treat classical and medieval Latin as points on one continuous curve rather than a legitimate standard and its corruption.
% DISAPPEARANCE_RATIONALE: If the rupture standard vanished, the humanist prestige economy built on demonstrated classical fluency would lose its scarcity basis, scholastic and technical Latin would be readable on their own terms without a corruption framing, and the print-academy curriculum industry certifying 'correct' Latin would need a different organizing principle — arrangements (patronage networks, curricula, canon formation) currently built around this hierarchy would need to reorganize.
% FOUNDING_PROBLEM: Fragmented, regionally divergent post-classical Latin usage made cross-regional humanist scholarly communication and the recovery/veneration of ancient texts difficult; a shared, textually-grounded standard was sought to unify educated communication around a prestigious, verifiable model.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists themselves attest the problem (fragmentation, need for a shared elite register) is real and remains live. Historians of medieval law and administration, writing from outside the humanist beneficiary set, attest that scholastic and notarial Latin were functionally coherent, stable registers serving their own communicative needs — the 'corruption' framing corroborated only from inside the humanist tradition itself, with no independent contemporary attestation that the medieval register was communicatively failing prior to the rupture standard's imposition.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) and rising across the interval because the rupture reading's operation is cumulative: each generation of humanist pedagogy further entrenches the classical-only standard and further devalues medieval-register output, an effect compounding as print technology scales the standard's reach. Suppression is authored substantial (0.71) and also rising, tracking the growing institutional machinery (universities, print curricula, court chancery style-guides) needed to enforce the standard against persistent scholastic and notarial practice that does not disappear on its own. Theater ratio rises moderately (0.42) as later humanist pedagogy increasingly performs classical purity (elaborate philological correctness contests, style anxiety) beyond what the original recovery project required. Accessibility collapse (0.62) and resistance (0.58) are both moderate-high: technical and legal registers persist stubbornly because classical Latin genuinely lacks the vocabulary they need, so alternatives do not fully collapse, but resistance is real and sustained rather than trivial.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist philologist seat, the standard looks like genuine intellectual recovery — restoring something real that had been lost, verifiable against surviving texts. From the medieval scholastic writer or notarial seat, the identical structure operates as retroactive delegitimization: a standard invented after their tradition existed, applied backward to declare their accumulated practice illegitimate, with no forum in which they can contest the framing (they are mostly dead, and the living technical scribes lack access to humanist polemical venues). The engine should register a Tangled Rope precisely because the coordination function (a shared elite register enabling cross-regional humanist communication) is real, while the extraction (devaluing centuries of scholastic and technical labor to concentrate prestige in classical fluency) rides on the same structure and requires the enforcement apparatus (curricula, style-policing, canon formation) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and Ciceronian stylists sit near the full-beneficiary end: they set the terms of correctness and their existing skill investment is what the standard rewards. Print academies benefit as intermediaries selling access to the standard. Medieval scholastic writers sit near the full-target end: trapped by biography (mostly deceased, unable to retrain or respond) and by the standard's explicit backward-application to their already-completed body of work. Vernacular-adjacent technical scribes are pushed even further toward target because their exit is not merely difficult but structurally impossible — no amount of effort produces classical vocabulary for concepts classical Latin never needed. Notaries occupy an intermediate position: constrained rather than trapped, because legal institutions retain some leverage to defend technical registers on functional grounds, but still bear real reputational cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading's founding problem (fragmented post-classical usage impeding humanist cross-regional communication) may have been genuinely live at the standard's origin, but its status is authored contested: the standard's persistence through subsequent centuries (rising theater_ratio, rising suppression_requirement) increasingly serves prestige-maintenance and curriculum-market functions rather than the original communicative coordination problem. Classifying this as Tangled Rope rather than outright Snare preserves the fact that a real coordination function existed (shared elite register) while still registering the asymmetric extraction that rides on it — collapsing it to Snare would erase the genuine coordination benefit to humanist scholars; collapsing it to Rope would erase the documented victim set and enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_recovery_vs_retroactive_construction,
    'Is the classical standard a genuine recovery of a real, previously-existing linguistic state, or is it substantially a Renaissance-era construction retroactively projected onto antiquity and then used to delegitimize the intervening centuries?',
    'Comparative philological analysis of surviving classical manuscripts against humanist reconstructions, checking for humanist-era emendations, invented ''classical'' forms not actually attested in surviving ancient sources, and selective canon formation (privileging Cicero over other classical registers that were themselves internally diverse).',
    'If substantially constructed rather than recovered, the rupture reading''s core legitimacy claim (fidelity to an actual ancient standard) weakens considerably, strengthening a reading of the constraint as manufactured extraction rather than restoration; if substantially genuine recovery, the coordination function is more robust and the Tangled Rope classification''s coordination half is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_recovery_vs_retroactive_construction, empirical, 'Whether the reconstructed classical standard is genuine historical recovery or retroactive humanist construction.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the rupture, continuity, and hybrid readings of the latin_correctness kernel disagree — is it about the empirical linguistic facts of change over time, about which register counts as authoritative, or about whether authority requires a single fixed point at all?',
    'Cross-reading structural comparison: continuity_reading treats classical and medieval Latin as points on one evolving curve with no privileged cut; hybrid_reading accepts a privileged classical cut but only for a subset of domains; rupture_reading accepts a privileged classical cut for all domains. The disagreement is located in whether a synchronic ''correctness'' standard is domain-total, domain-partitioned, or dissolved into diachronic continuity — not primarily in disputed facts about how Latin actually changed.',
    'Locating the disagreement in domain-scope (total vs partitioned vs none) rather than in empirical dispute about language change clarifies that the three readings are not competing for the same evidence — they are competing normative framings of largely agreed-upon historical facts, which is why they can coexist as live positions rather than one being simply refuted by evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Structural location of disagreement among sibling readings of the latin_correctness kernel.').

omega_variable(
    technical_register_unattainability,
    'Is classical purity genuinely structurally unattainable for technical/legal domains (because classical Latin lacks the relevant vocabulary), or could sufficiently motivated humanist scholarship have produced classicizing technical neologisms, meaning the ''unattainability'' is itself a choice not to invest effort rather than a hard structural limit?',
    'Survey humanist-era attempts (if any) at classicizing technical vocabulary in law, medicine, and commerce; compare uptake and success rate against the scale of the problem.',
    'If genuinely unattainable, the victimization of technical scribes is a harder structural fact (the standard could never have accommodated them); if merely under-invested, it suggests the humanist program deprioritized technical domains rather than being unable to serve them, which would sharpen rather than soften the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_register_unattainability, empirical, 'Whether technical-domain exclusion from classical purity is structural impossibility or humanist under-investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(lati_tr_t0, observed).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__rupture_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(lati_tr_t40, observed).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__rupture_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(lati_tr_t80, observed).
narrative_ontology:measurement(lati_tr_t120, latin_correctness__rupture_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement_basis(lati_tr_t120, observed).
narrative_ontology:measurement(lati_tr_t160, latin_correctness__rupture_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement_basis(lati_tr_t160, observed).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__rupture_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement_basis(lati_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(lati_be_t0, observed).
narrative_ontology:measurement(lati_be_t40, latin_correctness__rupture_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(lati_be_t40, observed).
narrative_ontology:measurement(lati_be_t80, latin_correctness__rupture_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement_basis(lati_be_t80, observed).
narrative_ontology:measurement(lati_be_t120, latin_correctness__rupture_reading, base_extractiveness, 120, 0.72).
narrative_ontology:measurement_basis(lati_be_t120, observed).
narrative_ontology:measurement(lati_be_t160, latin_correctness__rupture_reading, base_extractiveness, 160, 0.76).
narrative_ontology:measurement_basis(lati_be_t160, observed).
narrative_ontology:measurement(lati_be_t200, latin_correctness__rupture_reading, base_extractiveness, 200, 0.78).
narrative_ontology:measurement_basis(lati_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(lati_su_t0, observed).
narrative_ontology:measurement(lati_su_t40, latin_correctness__rupture_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(lati_su_t40, observed).
narrative_ontology:measurement(lati_su_t80, latin_correctness__rupture_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(lati_su_t80, observed).
narrative_ontology:measurement(lati_su_t120, latin_correctness__rupture_reading, suppression_requirement, 120, 0.65).
narrative_ontology:measurement_basis(lati_su_t120, observed).
narrative_ontology:measurement(lati_su_t160, latin_correctness__rupture_reading, suppression_requirement, 160, 0.69).
narrative_ontology:measurement_basis(lati_su_t160, observed).
narrative_ontology:measurement(lati_su_t200, latin_correctness__rupture_reading, suppression_requirement, 200, 0.71).
narrative_ontology:measurement_basis(lati_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.1).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the latin_correctness kernel, decomposed per the ε-invariance principle because the three readings assign structurally different beneficiary/victim sets and different ε values to what is colloquially called 'the correctness of Latin.' The rupture_reading (this story) authors high, domain-total extractiveness (0.78) because it delegitimizes all medieval usage across all domains. The continuity_reading would author near-zero extractiveness, treating classical and medieval Latin as one continuous, non-ruptured tradition with no corruption event and thus no victim set. The hybrid_reading would author a domain-partitioned ε — high for literary/rhetorical registers, near-zero for technical/practical registers — producing a different, smaller victim set (excluding technical scribes and notaries, who retain legitimacy under that reading). All three stories should be read together as a constraint family; none is the 'correct' measurement of a single underlying constraint — each is a distinct, ε-invariant constraint corresponding to a distinct normative reading of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
