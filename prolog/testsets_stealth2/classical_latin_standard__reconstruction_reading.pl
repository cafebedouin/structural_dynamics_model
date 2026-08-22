% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Reconstructionist Classical Latin Standard (Humanist Philological Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   Between roughly 1400 and 1600, the humanist movement redefined correct
 *   Latin: no longer the living form transmitted through unbroken scholastic
 *   and chancery practice, but the Classical form of Cicero, Virgil, Terence,
 *   and Caesar, recoverable only by returning to manuscripts and rejecting
 *   intervening usage as corruption. This story instantiates that
 *   reconstruction reading of the classical_latin_standard kernel as a clean,
 *   epsilon-invariant constraint: one standard, one beneficiary/victim
 *   structure, one classification. The standard solved a real coordination
 *   problem — a fixed, verifiable textual corpus replaced regionally drifting
 *   transmitted usage and restored rhetorical capacities that had genuinely
 *   lapsed — while simultaneously transferring linguistic authority,
 *   employability, and institutional position from practitioners whose
 *   competence rested on transmitted practice to a new gatekeeping class
 *   trained in philological method. Enforcement ran through the studia
 *   humanitatis curriculum, patronage networks, the corrected-editions print
 *   market, and sustained public ridicule of 'barbarous' Latinity. CONSTRAINT
 *   FAMILY NOTE: the colloquial label 'correct Latin' decomposes into three
 *   structurally distinct constraints — this reconstruction reading (high
 *   suppression of transmitted practice, high extraction), the continuity
 *   reading (drift legitimized as development, low extraction), and the
 *   hybrid reading (Classical core plus licensed technical developments,
 *   intermediate). Their epsilon values differ widely; each is authored as a
 *   separate file and linked via network.affects_constraints. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   while the metrics are authored descriptively from the structural record —
 *   the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - - humanist_pedagogues: Agenda-setter (organized/identity_locked) — administers the standard through schools and examinations and collects position, income, and prestige from it
 *   - - noble_patrons_of_letters: Primary beneficiary (powerful/arbitrage) — funds the movement and collects courtly prestige; exits cheaply
 *   - - classical_edition_printers: Secondary beneficiary (powerful/mobile) — owns the corrected-editions market the standard creates
 *   - - scholastic_university_masters: Primary target (organized/identity_locked) — bears the delegitimization of a lifetime's transmitted expertise
 *   - - ecclesiastical_chancery_clerks: Secondary target (organized/constrained) — bears the displacement of the ecclesiastical register they are employed to write
 *   - - provincial_schoolmasters: Diffuse target (powerless/trapped) — bears the harshest costs with the fewest defenses
 *   - - vernacular_authors: Excluded voice (moderate/mobile) — affected by the prestige hierarchy but absent from the forums where the standard is defined
 *   - - linguistic_historians: Analytical observer (analytical/analytical) — evaluates the full structure from outside the quarrel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.72).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Reconstructionist Classical Latin Standard (Humanist Philological Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '558035ad-5023-420f-b083-5f07c543b6ad').
narrative_ontology:cs_kernel_codification('558035ad-5023-420f-b083-5f07c543b6ad', fixed_text).
narrative_ontology:cs_authority_grounding('558035ad-5023-420f-b083-5f07c543b6ad', lineage).
narrative_ontology:cs_interpretation_layer_present('558035ad-5023-420f-b083-5f07c543b6ad').
narrative_ontology:cs_reading_relation('558035ad-5023-420f-b083-5f07c543b6ad', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('558035ad-5023-420f-b083-5f07c543b6ad', classical_latin_standard__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('558035ad-5023-420f-b083-5f07c543b6ad', foundational, classical_supremacy_intrinsic).
narrative_ontology:cs_axiom_status(classical_supremacy_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('558035ad-5023-420f-b083-5f07c543b6ad', classical_supremacy_intrinsic, deontological).
narrative_ontology:cs_axiom('558035ad-5023-420f-b083-5f07c543b6ad', foundational, practice_transmission_is_corruption).
narrative_ontology:cs_axiom_status(practice_transmission_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('558035ad-5023-420f-b083-5f07c543b6ad', practice_transmission_is_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('558035ad-5023-420f-b083-5f07c543b6ad', classical_golden_age_norm).
narrative_ontology:cs_drift_state('558035ad-5023-420f-b083-5f07c543b6ad', late_medieval_present, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('558035ad-5023-420f-b083-5f07c543b6ad', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_pedagogues).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, noble_patrons_of_letters).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, classical_edition_printers).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, scholastic_university_masters).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_chancery_clerks).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, provincial_schoolmasters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach Latin in the new studia humanitatis schools and university chairs; select the authors read, compose the grammars, examine pupils for Ciceronian purity, and write the letters of recommendation that determine preferment. Income, standing, and vocation all rest on the standard they teach; leaving it would mean abandoning the mission that defines their careers, so none do.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_pedagogues, agenda_setter,
    organized, biographical, identity_locked, continental).

% Princes, cardinals, and city oligarchs endow humanists, commission classical editions, and collect the prestige of recovered eloquence at their courts and in their chanceries. Patronage is discretionary wealth: a patron weary of the antique fashion redirects funds to painting or building without personal loss.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, noble_patrons_of_letters, beneficiary,
    powerful, biographical, arbitrage, continental).

% Print corrected texts of Cicero, Virgil, Terence, and Caesar together with commentaries, school editions, grammars, and dictionaries. The standard creates a durable market for authoritative editions; capital moves to whatever sells, and if demand shifted the presses would follow it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, classical_edition_printers, beneficiary,
    powerful, immediate, mobile, continental).

% Hold chairs in theology, law, and arts earned through decades of disputation training in transmitted scholastic Latin. Under the new standard their lecturing idiom, technical vocabulary, and quaestio format are publicly ridiculed as barbarous; keeping position means retraining mid-career in a style their formation never taught, while their expertise in the old curriculum becomes unsellable. Their commitment spans the generations of students they transmit the method to, which is exactly what the new standard interrupts.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, scholastic_university_masters, payer,
    organized, generational, identity_locked, continental).

% Draft curial, diocesan, and legal documents in the traditional ecclesiastical register handed down through their offices. Humanist criticism of chancery style pressures their employers to hire classically trained secretaries; an individual clerk can neither alter the register the office requires nor easily leave a salaried post built on that register.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, ecclesiastical_chancery_clerks, payer,
    organized, biographical, constrained, continental).

% Run village and small-town grammar schools teaching Latin from memorized late-medieval grammars and glossaries. As towns and bishops replace them with humanist-trained teachers, their methods are dismissed as ignorance, their livelihoods depend on local patronage they cannot influence, and retraining is financially out of reach.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, provincial_schoolmasters, payer,
    powerless, biographical, trapped, regional).

% Write in Italian, French, and other vernaculars and so stand outside the quarrel over Latin correctness, yet the prestige hierarchy the standard creates ranks their choice of tongue as a confession of incapacity. They hold no seat in the academies and print prefaces where the standard is defined; their practical exit — writing in the vernacular — is precisely what marks them as outsiders to it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, vernacular_authors, excluded,
    moderate, biographical, mobile, national).

% Later analysts reconstruct how the standard was imposed, what medieval Latin actually was, and what the recovery of Classical texts involved. They hold no stake in the quarrel and can evaluate all three readings of the standard from outside it.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_pedagogues).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored norm for written Latin across Europe: a fixed corpus against which any writer's Latin can be checked, replacing regionally and temporally variable transmitted usage with a verifiable standard, and restoring rhetorical registers that transmitted practice had lost.
% TRANSFER_FUNCTION: Moves linguistic authority, employability, and institutional position from practitioners whose competence rests on unbroken transmitted practice to specialists trained in philological method; moves educational resources and student fees toward schools teaching Classical authors; moves prestige to patrons who fund the recovery.
% ABSENT_VOICES: The practitioner communities being judged — scholastic masters, chancery clerks, liturgical users, village schoolmasters — had no seat where the standard was defined; definition happened in humanist academies, print prefaces, and patronage circles. The medieval authors whose usage is classified as corruption cannot answer at all. Vernacular writers, ranked by the resulting hierarchy, were likewise absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the reconstructionist standard vanished overnight, universities, chanceries, and schools would continue on transmitted usage; the humanist career structure, the corrected-editions market, and the grammar-school curriculum built on Classical authors would dissolve; and the boundary between 'correct' and 'corrupt' Latin — with all the status flows crossing it — would disappear with it.
% FOUNDING_PROBLEM: The perceived degradation of Latin from its Classical perfection through centuries of drift — barbarous syntax, corrupted vocabulary, loss of rhetorical power — and the problem of recovering the authentic form from the textual record.
% FOUNDING_PROBLEM_CORROBORATION: Continuity-tradition masters and church authorities attest from outside the benefiting parties that transmitted Latin served their purposes and that 'degradation' is a humanist construction; later historical linguistics corroborates that medieval registers were rule-governed and functional, undercutting the corruption premise, while confirming the factual existence of the textual record the reading relies on. No source outside the beneficiary set attests the founding problem as stated — that absence is itself signal.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the standard systematically converted practice-based competence into deficit: retraining costs, exclusion from preferment, and public ridicule fell on those least able to absorb them, while the gains accrued to a defined class. The referent is the reconstructionist standard as it actually operated on the population subject to it — not the continuity arrangement it displaced, and not an idealized version of itself. Suppression (0.78) reflects enforcement through curriculum control, patronage gatekeeping, print authority, and social delegitimization of alternatives as 'corruption' rather than mere unfashionableness; it is roughly 60 percent structural and 40 percent internalized (see the suppression omega). Theater ratio (0.32) is moderate-low: manuscript collation and critical edition were genuinely productive scholarship, but a growing share of activity became ritualized Ciceronian imitation and purity policing detached from communicative function. Accessibility collapse (0.52) is partial — ecclesiastical and legal registers persisted autonomously for centuries, so alternatives narrowed but did not vanish. Resistance (0.60) was sustained: scholastic counterattack, the Ciceronian controversy, and the institutional inertia of universities and chanceries slowed consolidation without stopping it. All three tracked metrics run on one shared time grid (1400/1440/1480/1520/1560/1600) so every metric is authored at every examined point; trajectories are monotonic consolidation, not cyclical, so no intermittent-reinforcement reading applies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the pedagogues' position the standard is restoration: the recovery of eloquence, the discipline of textual truth, a career-defining mission. From the scholastic masters' position the same structure is expropriation: decades of disputation-trained expertise reclassified overnight as barbarism, with retraining priced beyond a mid-career salary. Patrons and printers experience the standard as opportunity with cheap exit; provincial schoolmasters experience it as ruin with none. The engine computes these divergent per-seat classifications from power, exit options, and declared role — the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: patrons (arbitrage exit, discretionary wealth) sit nearest d=0; printers (mobile capital) close behind; pedagogues collect directly as administrators. Targets sit near the full-target end: scholastic masters combine victim status with identity_locked exit, placing them near d=1.0 — identity lock amplifies effective extraction for trapped targets; chancery clerks are constrained (salaried posts, employer-controlled register); provincial schoolmasters are trapped and powerless, near full target despite minimal power, because trapping amplifies d independently of power. No directionality overrides are authored: the declared beneficiary/victim structure plus differentiated exit options already separates the seats, and overrides are keyed by power atom, which would smear across heterogeneous agents sharing an atom (three 'organized' actors with opposed positions). Vernacular authors are excluded voices, not declared victims, so they feed no directionality — their absence from the derivation is itself the finding the absent_voices answer records.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Calling this a snare would erase the genuine coordination achievement — a fixed textual norm demonstrably improved precision and restored lost rhetorical capacity, which is why the standard outlived its founding generation. Calling it a rope would erase the systematic asymmetry — the same structure that coordinated communication simultaneously transferred authority, income, and standing from one class to another and required active enforcement to hold. Tangled_rope preserves both facts. On the genealogy interview: founding_problem_status is 'contested' (the continuity party denies the founding problem exists at all) and disappearance_verdict is 'world_rearranges', so the mismatch consumer finds no dead-mandate-plus-dependence flag — within the interval the arrangement's function was live and consolidating, not atrophied, and mandatrophy_resolved is accordingly not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_question,
    'Which reading of the classical_latin_standard kernel governed actual institutional practice during the interval — reconstruction, continuity, or hybrid?',
    'Curricular statutes, chancery style evolution, and university appointment records reveal which reading''s carriers controlled teaching and administration at each period.',
    'If continuity or hybrid governed practice, this story''s victim set and suppression profile misattribute costs that belong to the sibling constraints; each reading carries its own epsilon, beneficiary/victim structure, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operative_reading_question, conceptual, 'Committer-frame routing: this story instantiates one reading of a contested kernel; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    drift_degradation_or_development,
    'Is post-Classical linguistic change degradation requiring correction, or neutral development carrying no correctness valence?',
    'Comparative-historical assessment of whether scholastic and chancery registers lost functional capacity for their purposes or merely changed; modern historical linguistics treats change as neither progress nor decay.',
    'If neutral development, the delegitimization of practitioners loses its warrant and the arrangement reads as rent-seeking riding a real standard (snare-flavored); if genuine capacity loss, the coordination justification strengthens and extraction reads as the price of restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_degradation_or_development, empirical, 'Empirical basis of the corruption premise that distinguishes this reading from its siblings.').

omega_variable(
    standard_vs_gatekeeping_persistence,
    'Does the standard persist because a fixed textual norm genuinely coordinates learned communication, or because the philological class enforces the position it created?',
    'Counterfactual adoption analysis: would textual-standard convergence have emerged without humanist institutional power (for example through chancery standardization alone)? Compare regions with weak humanist presence.',
    'If enforcement explains persistence, excess extraction is gatekeeping rent; if coordination value explains it, the measured extractive component is overstated and the rope side of the hybrid is heavier than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_vs_gatekeeping_persistence, conceptual, 'Coordination-value versus gatekeeping-rent account of the standard''s persistence.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (curriculum control, patronage gatekeeping, employability) or internalized (shame and anxiety about incorrect Latinity that persists after barriers lift)?',
    'Post-exit trajectory of practitioners who left the system: if correctness-anxiety persists after institutional pressure ends, the internalized component is substantial.',
    'If heavily internalized, effective suppression exceeds the structural measure — targets carry the standard''s policing with them after exit; authored estimate is roughly 60 percent structural, 40 percent internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split for this reading''s enforcement.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the kernel the fixed Classical texts themselves, or the philological method that adjudicates them — and does the choice change the commitment-system classification?',
    'Test both framings: fixed_text kernel with lineage authority (authored here) versus formalized-method kernel with expertise authority; compare the resulting commitment-system patterns.',
    'Under the method-as-kernel framing, authority_grounding shifts toward expertise and the interpretation-layer semantics change; the declared framing was chosen because the reading''s own rhetoric grounds legitimacy in the texts, not the method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: text-kernel versus method-kernel framings yield different authority structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 1400, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_reconstruction_tr_t1400, classical_latin_standard__reconstruction_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(cls_reconstruction_tr_t1440, classical_latin_standard__reconstruction_reading, theater_ratio, 1440, 0.18).
narrative_ontology:measurement(cls_reconstruction_tr_t1480, classical_latin_standard__reconstruction_reading, theater_ratio, 1480, 0.22).
narrative_ontology:measurement(cls_reconstruction_tr_t1520, classical_latin_standard__reconstruction_reading, theater_ratio, 1520, 0.26).
narrative_ontology:measurement(cls_reconstruction_tr_t1560, classical_latin_standard__reconstruction_reading, theater_ratio, 1560, 0.3).
narrative_ontology:measurement(cls_reconstruction_tr_t1600, classical_latin_standard__reconstruction_reading, theater_ratio, 1600, 0.32).

% Extraction over time
narrative_ontology:measurement(cls_reconstruction_be_t1400, classical_latin_standard__reconstruction_reading, base_extractiveness, 1400, 0.42).
narrative_ontology:measurement(cls_reconstruction_be_t1440, classical_latin_standard__reconstruction_reading, base_extractiveness, 1440, 0.5).
narrative_ontology:measurement(cls_reconstruction_be_t1480, classical_latin_standard__reconstruction_reading, base_extractiveness, 1480, 0.58).
narrative_ontology:measurement(cls_reconstruction_be_t1520, classical_latin_standard__reconstruction_reading, base_extractiveness, 1520, 0.66).
narrative_ontology:measurement(cls_reconstruction_be_t1560, classical_latin_standard__reconstruction_reading, base_extractiveness, 1560, 0.7).
narrative_ontology:measurement(cls_reconstruction_be_t1600, classical_latin_standard__reconstruction_reading, base_extractiveness, 1600, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cls_reconstruction_su_t1400, classical_latin_standard__reconstruction_reading, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement(cls_reconstruction_su_t1440, classical_latin_standard__reconstruction_reading, suppression_requirement, 1440, 0.58).
narrative_ontology:measurement(cls_reconstruction_su_t1480, classical_latin_standard__reconstruction_reading, suppression_requirement, 1480, 0.65).
narrative_ontology:measurement(cls_reconstruction_su_t1520, classical_latin_standard__reconstruction_reading, suppression_requirement, 1520, 0.71).
narrative_ontology:measurement(cls_reconstruction_su_t1560, classical_latin_standard__reconstruction_reading, suppression_requirement, 1560, 0.75).
narrative_ontology:measurement(cls_reconstruction_su_t1600, classical_latin_standard__reconstruction_reading, suppression_requirement, 1600, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'correct Latin' (epsilon-invariance principle): the label conflates three structurally distinct claims. This reconstruction reading carries high epsilon (systematic delegitimization of practice-based authority, creation of a gatekeeping class); the continuity reading carries low epsilon (drift legitimized, no victim set beyond taste disputes); the hybrid reading sits between (licensed technical developments, partial extraction). The reconstruction reading is downstream in rhetorical dependence — its proponents cite the same textual corpus the siblings accept but deny legitimacy to the transmitted practice the other two build on. Each member links the others via affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
