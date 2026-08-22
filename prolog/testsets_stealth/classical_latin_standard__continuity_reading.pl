% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Living-Transmission Standard for Correct Latin (Continuity Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   Under the continuity reading, correct Latin is not a frozen classical
 *   artifact but the living form handed down through unbroken practice: what
 *   the transmitting community writes and speaks is correct, and natural
 *   drift absorbed along the way is legitimate development rather than
 *   corruption. The arrangement genuinely coordinates a transnational learned
 *   community across space and time, and it simultaneously gates access:
 *   legitimacy of usage flows only through the recognized transmission
 *   channels, so idiom formed outside them is marked faulty regardless of
 *   quality. The same structure that lets insiders' innovations count as
 *   development makes outsiders' identical innovations count as barbarism.
 *   KEY AGENTS (by structural relationship): - clerical_latin_establishment:
 *   agenda_setter and principal collector (institutional/identity_locked) —
 *   administers formation, certifies usage, its authority is the lineage -
 *   humanist_academic_faculties: beneficiary (organized/constrained) —
 *   collects standing and refereeing power without running the machinery -
 *   living_latin_pedagogues: beneficiary with stewardship costs
 *   (moderate/constrained) - text_reconstructed_latinists: payer
 *   (moderate/constrained) — idiom marked artificial absent lineage formation
 *   - extra_institutional_latin_learners: payer (powerless/constrained) — no
 *   certification path short of institutional entry -
 *   vernacular_medium_educators: excluded (organized/mobile) — would redirect
 *   the standard's resource base - comparative_linguistics_community:
 *   analytical observer — documents drift, adjudicates
 *   development-versus-fault disputes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.42).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.22).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Living-Transmission Standard for Correct Latin (Continuity Reading)").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, 'af1975a3-9e45-4b01-9f61-678fa53aca41').
narrative_ontology:cs_kernel_codification('af1975a3-9e45-4b01-9f61-678fa53aca41', implicit).
narrative_ontology:cs_authority_grounding('af1975a3-9e45-4b01-9f61-678fa53aca41', practice).
narrative_ontology:cs_interpretation_layer_present('af1975a3-9e45-4b01-9f61-678fa53aca41').
narrative_ontology:cs_reading_relation('af1975a3-9e45-4b01-9f61-678fa53aca41', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('af1975a3-9e45-4b01-9f61-678fa53aca41', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('af1975a3-9e45-4b01-9f61-678fa53aca41', foundational, consuetudo_constitutes_correctness).
narrative_ontology:cs_axiom_status(consuetudo_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('af1975a3-9e45-4b01-9f61-678fa53aca41', consuetudo_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('af1975a3-9e45-4b01-9f61-678fa53aca41', foundational, drift_is_legitimate_development).
narrative_ontology:cs_axiom_status(drift_is_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('af1975a3-9e45-4b01-9f61-678fa53aca41', drift_is_legitimate_development, instrumental).
narrative_ontology:cs_reference_frame('af1975a3-9e45-4b01-9f61-678fa53aca41', unbroken_practice_continuum).
narrative_ontology:cs_drift_state('af1975a3-9e45-4b01-9f61-678fa53aca41', contemporary_post_vernacular_turn, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af1975a3-9e45-4b01-9f61-678fa53aca41', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, clerical_latin_establishment).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, humanist_academic_faculties).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, living_latin_pedagogues).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, text_reconstructed_latinists).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, extra_institutional_latin_learners).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, usage_as_arbiter_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, consuetudo_as_norm_regula).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, organic_drift_development_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the formation-and-certification machinery through which acceptable Latin is transmitted: seminaries, chancery schools, curial drafting offices, and the teaching orders that train each generation of users. It decides which new constructions count as legitimate development and which are corrected as faults, and its own current usage is self-certifying because it is the lineage. Its institutional authority is constituted by its claim to carry the unbroken transmission; abandoning that claim would dissolve the ground on which its standing rests.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, clerical_latin_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, clerical_latin_establishment, beneficiary).

% University classics, theology, and history faculties formed inside the practice collect careers, editorial authority, and refereeing power from the standard without administering it centrally. Their members' own idiom, having been formed in the lineage, is accepted as developed rather than faulty. Leaving the standard would forfeit their standing as custodians of the inherited language.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, humanist_academic_faculties, beneficiary,
    organized, biographical, constrained, continental).

% Teachers of immersion-based Latin instruction collect a professional raison d'etre and a market niche from the claim that the language lives through practice. They also carry real maintenance costs: sustaining fluent spoken use, training themselves to model the accepted idiom, and policing the line between development and fault in their pupils' work.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, living_latin_pedagogues, beneficiary,
    moderate, biographical, constrained, regional).

% Scholars and accomplished amateurs whose Latin idiom was built from grammars, lexica, and close reading of the classical corpus rather than from formation inside a practicing community. Under the living-transmission standard their prose is marked as artificial or bookish regardless of its grammatical accuracy, and full standing requires costly re-formation through the recognized channels. Their realistic exit is allegiance to a rival standard built on textual recovery, at the price of standing in the dominant institutions.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, text_reconstructed_latinists, payer,
    moderate, biographical, constrained, continental).

% Autodidacts, students outside seminary and faculty channels, and learners in institutions without recognized lineage standing. However well they write, no authority within the arrangement can certify their usage as developed rather than faulty, because certification flows only through the transmission chain. Their options are expensive entry into the forming institutions, quiet abandonment of the ambition, or exit from Latin altogether.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, extra_institutional_latin_learners, payer,
    powerless, biographical, constrained, global).

% Advocates and administrators of vernacular-language schooling who argue that the learned functions Latin held should be carried by national languages and, latterly, by English. They sit outside the councils where the Latin standard is maintained and would redirect the resources that sustain it; their exclusion from the conversation is what leaves the standard's resource base unexamined by its main competitors.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_medium_educators, excluded,
    organized, generational, mobile, national).

% Historical linguists and sociolinguists who document how the standard actually evolved, measure the shrinking volume of functional use against ceremonial use, and adjudicate disputes about where legitimate development ends. They take testimony from every seat and can demonstrate, for instance, that features once condemned as faults became sanctioned once the practicing community adopted them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, comparative_linguistics_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, clerical_latin_establishment).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a mutually intelligible supraregional register across borders and centuries for law, liturgy, scholarship, and learned correspondence, so that each generation inherits usable competence through teachers and texts in circulation instead of re-deriving correctness from fragmentary ancient evidence.
% TRANSFER_FUNCTION: Moves certification of linguistic legitimacy from outsiders to lineage-formed incumbents; moves the cost of acquisition onto entrants, who must spend years inside recognized formation channels; converts incumbent usage into self-certifying authority over what counts as development versus fault.
% ABSENT_VOICES: Vernacular-medium educators are structurally outside the maintenance councils. Historically, women and the colonized were barred from the seminaries and universities through which formation flowed, so the standard's consensus was formed without them even as their literate output was judged by it; ordinary worshippers received a liturgy they could not understand and had no seat in its defense. None of these seats participates in deciding what counts as legitimate development.
% DISAPPEARANCE_RATIONALE: If the living-transmission standard vanished overnight, the remaining functional uses would scramble: curial drafting, seminary formation, immersion pedagogy, and the living-Latin school network would lose their warrant and reorganize around either a textual-recovery standard or abandonment of the register; botanical, zoological, and pharmacological nomenclature communities would face pressure to re-found their codes; the accumulated claim that continuity itself confers authority would need replacement wherever it currently settles disputes.
% FOUNDING_PROBLEM: After the Western Empire's administrative collapse, regional speech diverged within generations, and the pan-European learned class risked losing any shared register for law, liturgy, and scholarship. The continuity solution held a register together by keeping transmission alive teacher-to-pupil and treating the resulting drift as the language legitimately developing rather than decaying.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medieval Latin corroborate from outside the beneficiary set that the fragmentation problem was real and that the transmission solution worked (Carolingian renaissance scholarship, studies of the cathedral and university schools). Sociolinguists of the nineteenth and twentieth centuries equally attest, from outside, that the supraregional-register function has largely migrated first to national vernaculars and then to English. The Church's own offices attest the problem remains live for liturgical and juridical use. No single external source attests both halves at once; the split testimony is itself the finding.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope because the structure verifiably does two things at once: it coordinates (a shared, evolving, mutually intelligible register that no codified grammar alone sustains) and it extracts asymmetrically (certification flows only through lineage channels, so incumbents' usage is self-certifying while outsiders' equivalent usage is discounted). The metrics describe operation, not aspiration. Extractiveness 0.42 is moderate: the gate is real but the toll is formation cost and standing discount rather than systematic expropriation. Suppression 0.22 is low because drift inside the lineage is welcomed — the standard absorbs novelty rather than forbidding it; only idiom with no lineage connection is excluded. Accessibility_collapse 0.35 reflects that alternatives persist: one can write Latin by reconstructionist lights, adopt the hybrid position, or leave for the vernaculars; the standard ranks alternatives without erasing them. Resistance 0.40 records five centuries of organized dissent from philological reconstructionists and vernacular advocates. The temporal series share one grid (1450-2025, seven points, all three metrics at every point). Extractiveness climbs through the Counter-Reformation disciplinary build-up and the nineteenth-century gymnasium systems, where Latin proficiency sorted classes, then eases as vernaculars absorb the functions. Suppression_requirement traces a genuine enforcement-capacity arc — loose humanist republic, Tridentine seminary and index ratchet peaking mid-seventeenth century, state-school takeover, post-conciliar decay to a mainly pedagogical residue — which is why that series is authored rather than left static. Theater_ratio rises monotonically as the functional domain contracts faster than ceremonial use (mottos, diploma formulas, recitation), reaching 0.48: the living core (curial drafting, nomenclature codes, immersion schools) remains real, but nearly half of contemporary Latin activity is performance of Latinity rather than use of it. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From inside the lineage — establishment, formed faculties, pedagogues — the arrangement presents as pure coordination: their drift is development, their authority is stewardship, and the gate looks like quality control. From the extra-lineage seats the identical structure presents as a gate that discounts their work irrespective of merit. The establishment seat additionally exhibits institutional identity lock: the organization has become its function as bearer of the transmission, so exit is not costly but conceptually unavailable — renouncing the continuity claim dissolves the ground of its own authority. If that identity frame broke (for instance, if the Church re-founded its Latin authority on textual criticism rather than transmission), the establishment's directionality would shift toward symmetric and the constraint's enforcement burden would migrate to whoever claimed the stewardship next. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. The establishment sits nearest the beneficiary end (d near 0): it collects certification authority and sets the rules, damped slightly by real stewardship costs. Faculties and pedagogues are beneficiaries with moderate damping — pedagogues least subsidized of the three because they carry the maintenance labor. Text-reconstructed latinists sit well toward the target end (d high): they bear the standing discount and the re-formation cost, with exit only into a weaker rival camp. Extra-institutional learners sit nearest the full-target end among the payers: powerless relative to the certifying apparatus, with no arbitration path, so whatever extraction the gate produces lands on them at full strength. Vernacular educators are excluded rather than targeted — the constraint does not extract from them, it ignores them while consuming the terrain they would claim. No directionality overrides are needed: beneficiary/victim declarations plus exit options already separate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding a supraregional learned register after vernacular fragmentation — was real and was substantially solved; the disputed part is whether it remains live. Classifying this as tangled_rope rather than snare prevents misreading a functioning coordination core (centuries of cumulative jurisprudence, liturgy, and scholarship run through it) as pure extraction; classifying it as anything purer than tangled_rope would erase the documented gate that discounts extra-lineage idiom. The R5 interview records the tension: founding_problem_status is contested (live for the Church's uses, dead for most scholarly functions) while disappearance_verdict is world_rearranges, so the mismatch consumer sees no automatic zombie flag — but the theater_ratio trajectory approaching 0.5 marks this constraint for piton-decay monitoring, and the dedicated omega makes that risk explicit rather than latent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the continuity reading capture the classical_latin_standard kernel, or is the kernel better instantiated by the reconstruction or hybrid reading?',
    'Corpus-level comparison of the three sibling stories'' computed classifications against the historical record of Latinity disputes (Ciceronian controversy, neohumanist philology, post-conciliar liturgy reform): whichever reading best predicts where enforcement actually concentrated and whom it actually excluded is the better kernel instantiation.',
    'If the reconstruction reading dominates, the victim set expands to nearly all post-Classical usage, suppression rises sharply, and the family''s center of gravity moves toward snare; if hybrid dominates, extraction concentrates in the boundary cases between textual fidelity and accepted development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the correct-Latin kernel the structural evidence best supports.').

omega_variable(
    natural_vs_constructed_transmission,
    'Is the living-transmission standard a natural feature of language transmission (every living language passes down through practice and drifts) or a constructed institutional arrangement whose gatekeeping benefits identifiable incumbents?',
    'Compare the Latin case against unwritten vernacular traditions that transmit without certifying institutions: if comparable drift acceptance arises without a formation gate, the drift-legitimacy half is natural and only the certification layer is constructed.',
    'If the certification layer is the constructed part, the constraint''s extractiveness attaches to that layer specifically and could in principle be stripped off (open certification) without losing the coordination function; if the whole arrangement is natural, intervention targets vanish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_transmission, conceptual, 'Naturality of the transmission standard versus constructedness of its gatekeeping layer.').

omega_variable(
    victim_set_minimality,
    'Is the victim set genuinely minimal — only idiom without lineage connection discounted — or do ascriptively excluded populations (historically women and the colonized barred from formation channels) constitute a systematically excluded class the scalar metrics undercount?',
    'Prosopographic study of who could access formation channels per era, correlated against whose Latin was condemned as faulty in the period''s corrective literature.',
    'If exclusion tracked ascriptive status rather than idiom, effective extraction on the excluded seats approaches full-target levels and the constraint''s classification shifts toward the extractive end despite low average suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_minimality, empirical, 'Whether the thin declared victim set hides systematic ascriptive exclusion.').

omega_variable(
    drift_barbarism_boundary_circularity,
    'Where does legitimate development end and barbarism begin, given that the criterion is retroactive sanction by the practicing community — the lineage legitimizes what it adopts and condemns what it does not?',
    'Test the boundary against pre-adoption condemnation records: catalog constructions first condemned as faults and later sanctioned as development, and measure whether any principled criterion predicted the outcome other than eventual community adoption.',
    'If no criterion exists, the exclusion of barbarisms is unfalsifiable from inside the lineage, and the gatekeeping function is revealed as pure incumbency protection — raising the extraction attributable to the certification layer specifically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drift_barbarism_boundary_circularity, conceptual, 'Circularity of the development-versus-barbarism criterion under the continuity reading.').

omega_variable(
    piton_decay_risk,
    'Will the continuity reading survive the contraction of Latin''s functional domain, or is it degrading into ceremonial maintenance of a transmission whose living substance has thinned?',
    'Track the ratio of functional to ceremonial Latin use (curial and nomenclature output, immersion-school enrollment versus motto and recitation volume) across coming decades; sustained theater_ratio above 0.5 with falling functional volume confirms decay.',
    'Confirmed decay reclassifies the constraint toward piton: theatrical continuity administered by an establishment that could restore substance but bears less cost from decline than from admitting it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_decay_risk, empirical, 'Whether living transmission is persisting or being replaced by performed continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 1450, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_continuity_tr_t1450, classical_latin_standard__continuity_reading, theater_ratio, 1450, 0.18).
narrative_ontology:measurement_basis(cls_continuity_tr_t1450, observed).
narrative_ontology:measurement(cls_continuity_tr_t1550, classical_latin_standard__continuity_reading, theater_ratio, 1550, 0.24).
narrative_ontology:measurement_basis(cls_continuity_tr_t1550, observed).
narrative_ontology:measurement(cls_continuity_tr_t1650, classical_latin_standard__continuity_reading, theater_ratio, 1650, 0.3).
narrative_ontology:measurement_basis(cls_continuity_tr_t1650, observed).
narrative_ontology:measurement(cls_continuity_tr_t1750, classical_latin_standard__continuity_reading, theater_ratio, 1750, 0.34).
narrative_ontology:measurement_basis(cls_continuity_tr_t1750, observed).
narrative_ontology:measurement(cls_continuity_tr_t1850, classical_latin_standard__continuity_reading, theater_ratio, 1850, 0.37).
narrative_ontology:measurement_basis(cls_continuity_tr_t1850, observed).
narrative_ontology:measurement(cls_continuity_tr_t1950, classical_latin_standard__continuity_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement_basis(cls_continuity_tr_t1950, observed).
narrative_ontology:measurement(cls_continuity_tr_t2025, classical_latin_standard__continuity_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(cls_continuity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cls_continuity_be_t1450, classical_latin_standard__continuity_reading, base_extractiveness, 1450, 0.3).
narrative_ontology:measurement_basis(cls_continuity_be_t1450, observed).
narrative_ontology:measurement(cls_continuity_be_t1550, classical_latin_standard__continuity_reading, base_extractiveness, 1550, 0.36).
narrative_ontology:measurement_basis(cls_continuity_be_t1550, observed).
narrative_ontology:measurement(cls_continuity_be_t1650, classical_latin_standard__continuity_reading, base_extractiveness, 1650, 0.46).
narrative_ontology:measurement_basis(cls_continuity_be_t1650, observed).
narrative_ontology:measurement(cls_continuity_be_t1750, classical_latin_standard__continuity_reading, base_extractiveness, 1750, 0.5).
narrative_ontology:measurement_basis(cls_continuity_be_t1750, observed).
narrative_ontology:measurement(cls_continuity_be_t1850, classical_latin_standard__continuity_reading, base_extractiveness, 1850, 0.52).
narrative_ontology:measurement_basis(cls_continuity_be_t1850, observed).
narrative_ontology:measurement(cls_continuity_be_t1950, classical_latin_standard__continuity_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement_basis(cls_continuity_be_t1950, observed).
narrative_ontology:measurement(cls_continuity_be_t2025, classical_latin_standard__continuity_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(cls_continuity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cls_continuity_su_t1450, classical_latin_standard__continuity_reading, suppression_requirement, 1450, 0.25).
narrative_ontology:measurement_basis(cls_continuity_su_t1450, observed).
narrative_ontology:measurement(cls_continuity_su_t1550, classical_latin_standard__continuity_reading, suppression_requirement, 1550, 0.4).
narrative_ontology:measurement_basis(cls_continuity_su_t1550, observed).
narrative_ontology:measurement(cls_continuity_su_t1650, classical_latin_standard__continuity_reading, suppression_requirement, 1650, 0.55).
narrative_ontology:measurement_basis(cls_continuity_su_t1650, observed).
narrative_ontology:measurement(cls_continuity_su_t1750, classical_latin_standard__continuity_reading, suppression_requirement, 1750, 0.52).
narrative_ontology:measurement_basis(cls_continuity_su_t1750, observed).
narrative_ontology:measurement(cls_continuity_su_t1850, classical_latin_standard__continuity_reading, suppression_requirement, 1850, 0.48).
narrative_ontology:measurement_basis(cls_continuity_su_t1850, observed).
narrative_ontology:measurement(cls_continuity_su_t1950, classical_latin_standard__continuity_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement_basis(cls_continuity_su_t1950, observed).
narrative_ontology:measurement(cls_continuity_su_t2025, classical_latin_standard__continuity_reading, suppression_requirement, 2025, 0.22).
narrative_ontology:measurement_basis(cls_continuity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'correct Latin'. The label conflates at least three structurally distinct legitimacy regimes: unbroken living transmission (this file, moderate extraction, minimal victims, low suppression), textual recovery through philological archaeology (reconstruction_reading — maximal suppression of post-Classical practice, vast victim set), and mediated synthesis (hybrid_reading — intermediate, extraction concentrated at the fidelity-versus-development boundary). Each member carries its own epsilon, beneficiary/victim structure, and enforcement profile; they are linked here so purity-contamination analysis tracks the family. The upstream continuity claim historically supplied the legitimacy conditions under which the hybrid's recognition of post-Classical developments became available, which is why this file declares an influences edge toward the hybrid sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
