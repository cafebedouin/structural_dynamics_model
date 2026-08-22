% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Domain-Partition Reading of Shinbutsu Coexistence (Kami/Buddha Functional Division)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This story authors the domain-partition reading of the shinbutsu-shugo
 *   kernel: the medieval-to-early-modern Japanese arrangement under which
 *   kami cults and Buddhist institutions coexisted by dividing existential
 *   jurisdiction — kami govern life, purity, and harvest; Buddhas govern
 *   death, salvation, and the afterlife — without either side needing to
 *   resolve how the two cosmologies relate at the level of ultimate reality.
 *   This is presented as one of three structurally distinct readings of the
 *   same historical kernel. The syncretic_fusion_reading (a separate story)
 *   holds instead that honji suijaku doctrine achieves genuine ontological
 *   unification, treating kami as local manifestations of universal Buddhas.
 *   The incoherent_bundle_reading (a separate story) holds that the whole
 *   arrangement was never coherent at all, sustained by institutional
 *   convenience and ambiguity rather than any stable logic,
 *   domain-partitioned or unified, and that this incoherence is precisely
 *   what let Meiji reformers dismantle it so completely. This story's ε
 *   (0.32, moderate-low, rising slowly) reflects the domain-partition
 *   reading's own account: institutional actors on both sides quietly benefit
 *   from not having to defend a unification claim, and that benefit
 *   accumulates gently over centuries as an entrenched division of ritual
 *   labor and revenue — but it never becomes the sharp doctrinal defense or
 *   violent enforcement that the fusion or incoherence readings would
 *   register.
 *
 * KEY AGENTS:
 *   - shrine_priests: institutional beneficiary of undisturbed kami jurisdiction
 *   - temple_clergy: institutional beneficiary of undisturbed Buddhist jurisdiction
 *   - village_households: practical beneficiaries who live the partition without theorizing it
 *   - systematic_theologians: bear the cost of unrealized unification projects
 *   - meiji_state_shinto_reformers: bear the cost of having to forcibly manufacture what this reading held loosely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.32).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Domain-Partition Reading of Shinbutsu Coexistence (Kami/Buddha Functional Division)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '1742ed0c-0700-4dac-b148-149791158036').
narrative_ontology:cs_kernel_codification('1742ed0c-0700-4dac-b148-149791158036', distributed).
narrative_ontology:cs_authority_grounding('1742ed0c-0700-4dac-b148-149791158036', practice).
narrative_ontology:cs_interpretation_layer_present('1742ed0c-0700-4dac-b148-149791158036').
narrative_ontology:cs_reading_relation('1742ed0c-0700-4dac-b148-149791158036', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1742ed0c-0700-4dac-b148-149791158036', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('1742ed0c-0700-4dac-b148-149791158036', foundational, existential_domains_are_jurisdictionally_separable).
narrative_ontology:cs_axiom_status(existential_domains_are_jurisdictionally_separable, holdable).
narrative_ontology:cs_axiom_grounding('1742ed0c-0700-4dac-b148-149791158036', existential_domains_are_jurisdictionally_separable, conventional).
narrative_ontology:cs_axiom('1742ed0c-0700-4dac-b148-149791158036', secondary, ontological_unification_is_unnecessary_for_ritual_coexistence).
narrative_ontology:cs_axiom_status(ontological_unification_is_unnecessary_for_ritual_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('1742ed0c-0700-4dac-b148-149791158036', ontological_unification_is_unnecessary_for_ritual_coexistence, instrumental).
narrative_ontology:cs_reference_frame('1742ed0c-0700-4dac-b148-149791158036', medieval_dual_jurisdiction_settlement).
narrative_ontology:cs_drift_state('1742ed0c-0700-4dac-b148-149791158036', meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1742ed0c-0700-4dac-b148-149791158036', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, village_households).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shugen_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, systematic_theologians).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_shinto_reformers).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_pluralism_without_ontological_unification).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, practice_based_religious_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites for purity, harvest, birth, and community protection. Under the domain-partition reading, they need no theological reconciliation with Buddhist doctrine to perform their function — kami matters are simply outside the Buddhist jurisdiction of death and salvation. This division protects their institutional turf and ritual monopoly over life-affirming rites without requiring them to subordinate kami to any Buddhist metaphysic.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priests, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priests, agenda_setter).

% Administer funerary rites, ancestor memorialization, and salvation doctrine. The domain-partition reading grants them exclusive jurisdiction over death and afterlife matters, undisturbed by kami claims, and lets them avoid the harder theological work of explaining how a universal Buddha relates to a territorially bound kami.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, temple_clergy, agenda_setter).

% Draw on kami rites for birth, marriage, and harvest, and on temple rites for funerals and ancestor worship, without needing either system to explain the other. They practice both without demanding doctrinal consistency; the partition matches how they actually live their ritual calendar, and no one asks them to resolve a contradiction they never experience as one.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_households, beneficiary,
    powerless, biographical, constrained, local).

% Mountain ascetics and itinerant ritualists who move between kami and Buddhist idioms depending on the occasion and the client's need — mountain purification here, mortuary rites there. The domain-partition reading legitimizes their boundary-crossing practice precisely because it denies that crossing requires ontological justification.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shugen_ritual_specialists, beneficiary,
    moderate, generational, mobile, regional).

% Scholars and doctrinal authorities (Buddhist and, later, kokugaku scholars) who want a single coherent account of how kami and Buddhas relate. The domain-partition reading treats their unification project as unnecessary, denying them the intellectual and institutional payoff of a resolved cosmology — their labor toward systematization finds no market.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, systematic_theologians, payer,
    moderate, civilizational, trapped, national).

% Nineteenth-century state actors who needed a cleanly separated, purified Shinto distinct from Buddhism to construct a national ideology (shinbutsu bunri). The domain-partition reading, ironically, supplies some of the separative logic they need, but their forcible administrative separation (destroying combinatory shrine-temple complexes) contradicts the earlier reading's tolerant, non-hierarchical coexistence — they pay by having to violently manufacture a boundary the older reading held loosely and without coercion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_shinto_reformers, payer,
    institutional, generational, arbitrage, national).

% Medieval exponents of the view that kami are local manifestations (suijaku) of universal Buddhist origins (honji) would object that the domain-partition reading understates the ontological unification their tradition actually achieved. They are not stakeholders of this constraint — they are the sibling reading's constituency and are absent from this reading's account by construction.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, honji_suijaku_theorists, excluded,
    moderate, civilizational, constrained, national).

% Study shrine-temple complexes (jingu-ji), the honji suijaku corpus, and Meiji-era separation records to reconstruct which reading best describes premodern practice at any given site and period. They observe considerable variation — some complexes leaned toward fusion, others toward strict functional division — without a single verdict covering the whole tradition.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two distinct ritual specialist classes (kami priests, Buddhist clergy) and the households they serve to divide existential labor by domain — life-affirming, purity, and harvest concerns to kami; death, salvation, and afterlife concerns to Buddhas — without requiring either side to subordinate its cosmology to the other's.
% TRANSFER_FUNCTION: Distributes ritual authority and its associated offerings/patronage along functional lines: kami institutions retain jurisdiction (and revenue) over birth/harvest/purity rites; Buddhist institutions retain jurisdiction (and revenue) over funerary/memorial/salvation rites. Little is transferred FROM one specialist class TO the other; the partition mostly protects each side's existing revenue base from doctrinal absorption by the other.
% ABSENT_VOICES: Honji suijaku theorists, who held that kami were literally manifestations of Buddhas, are excluded from this reading's account — their unification claims are the subject matter of the sibling syncretic_fusion_reading, not a voice within this one. Meiji state ideologues who wanted a purified, Buddhism-free Shinto are present as payers but their coercive separation project is not the same as this reading's tolerant non-unification.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, village ritual practice (drawing on kami for life-events and temples for death-events) would likely continue on inertia and habit even without the interpretive frame explaining why that division is coherent — practice does not obviously depend on the theory. But institutional actors (shrine and temple administrations) who currently justify their non-overlapping jurisdictions partly by appeal to domain separation would face renewed pressure either toward doctrinal fusion claims or toward the kind of forced administrative separation the Meiji state imposed. Historians disagree on how much of the historical stability was load-bearing versus incidental, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Medieval and early-modern Japan needed a way for two ritual traditions with different origins, personnel, and institutional bases (native kami cults and imported Buddhism) to operate in the same communities and often the same physical sites without either side's clergy having to defeat or absorb the other doctrinally.
% FOUNDING_PROBLEM_CORROBORATION: Shrine and temple administrations themselves attest the founding problem was real and remains functionally solved by the partition (they are the beneficiaries, so this is not independent). Independent corroboration is weaker: historians of religion (e.g., work on jingu-ji shrine-temple complexes) attest that PRACTICE across sites was heterogeneous — some complexes practiced deep ritual and administrative fusion, others practiced closer functional separation — meaning the domain-partition account describes real cases but was never the exclusive or uncontested pattern across the tradition as a whole.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate-low (0.32) because the domain-partition reading is fundamentally a low-friction coordination arrangement: two institutional systems avoid costly doctrinal conflict by simply not overlapping in claimed jurisdiction. Suppression is low (0.28) because the reading imposes almost no coercive apparatus — no inquisition into kami-Buddha ontology, no doctrinal enforcement, just practical non-interference. Theater ratio starts very low (0.10) and rises modestly (0.22) reflecting gradual institutional layering (temple-shrine complex administration, ritual calendars) that accrues some performative maintenance over thirteen centuries without ever becoming the dominant feature. Accessibility collapse is moderate (0.4): the domain division is not the only conceivable arrangement — fusion and incoherence readings existed simultaneously in the same historical record — but it was accessible and legible enough that ordinary households never needed to resolve it as a live problem. Resistance (0.35) captures the friction from theologians on both sides who periodically pushed for unification (honji suijaku) or, later, forcible separation (Meiji shinbutsu bunri), against the partition's default inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-priest and temple-clergy seats, this arrangement looks like a durable, low-cost rope: two systems, two jurisdictions, mutual non-interference, everybody keeps their revenue. From the systematic-theologian seat, the same arrangement looks like an evasion — a refusal to do the intellectual work that would either validate or falsify the coexistence, sustained because both institutional sides prefer the ambiguity to a resolution that might favor the other. The engine computes these divergent seat classifications from the same structural data; the domain-partition reading's own claimed_type (rope) describes the institutional experience, not a theologically settled verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priests and temple clergy sit near the beneficiary end of directionality: each retains an undisturbed jurisdictional monopoly and revenue base precisely because the domain partition asks nothing further of them. Village households are also beneficiaries — they draw on both systems as needed without incurring any cost from the lack of unification; the absence of theological resolution is invisible friction to them, not a burden. Systematic theologians and Meiji reformers sit toward the target end: their projects (doctrinal unification or forcible national separation) are structurally denied traction by a reading that treats non-unification as sufficient and stable. The shugen ritual specialists are a distinctive beneficiary case: their itinerant, boundary-crossing practice is legitimated precisely because the domain-partition reading does not require boundary-crossers to justify themselves ontologically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how two ritual traditions with different origins and personnel coexist in the same communities — remains at least partially live wherever both kami shrines and Buddhist temples continue to operate in the same localities (which is most of Japan). This is not a classic mandatrophy case of an obsolete arrangement propped up by inertia; the domain-partition reading claims the underlying coordination need never disappeared, only that the theological resolution never arrived (or was never required). Where the reading risks slipping into mandatrophy is in taking institutional convenience for structural necessity: the partition's persistence may owe more to habit and non-confrontation than to a genuine absence of overlap (site-level jingu-ji histories show real boundary contestation), which is exactly the tension the incoherent_bundle_reading presses on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_versus_fusion_site_variance,
    'Was functional domain-partition the dominant historical pattern, or did most shrine-temple complexes (jingu-ji) practice something closer to the fusion this reading denies?',
    'Systematic site-by-site historical survey of jingu-ji complexes across regions and centuries, coding each for degree of doctrinal fusion versus functional separation in recorded ritual practice and clergy organization.',
    'If fusion dominates the record, the domain-partition reading describes a minority or idealized pattern rather than the tradition''s central tendency, and the syncretic_fusion_reading becomes the better-fitting kernel reading for most sites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_versus_fusion_site_variance, empirical, 'Whether functional partition or doctrinal fusion was the historically dominant pattern across shrine-temple complexes.').

omega_variable(
    convenience_versus_coherence,
    'Does the domain-partition arrangement reflect a genuinely coherent non-unification commitment, or is it a convenient description imposed retrospectively on what was actually institutional avoidance of a hard theological question?',
    'Textual analysis of medieval clerical writings for explicit non-unification arguments versus silence/avoidance on the ontological question; presence of explicit argument would support coherence, near-total silence would support the incoherent_bundle_reading''s account.',
    'If clerics rarely if ever argued FOR domain separation as a positive doctrine (mostly just practiced non-interference), the domain-partition reading may overstate its own coherence relative to the incoherent_bundle_reading, which holds the whole arrangement together only through ambiguity rather than any stable logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convenience_versus_coherence, conceptual, 'Whether domain partition was an articulated doctrine or a retrospectively imposed coherence on institutional avoidance.').

omega_variable(
    meiji_separation_as_evidence,
    'Does the relative ease with which Meiji reformers forcibly separated kami and Buddhist institutions (shinbutsu bunri) count as evidence the domains were always separable (supporting this reading), or as evidence the prior arrangement was more entangled than this reading claims and separation required violence precisely because it was NOT a pre-existing clean partition?',
    'Comparative study of shrine-temple complexes that resisted separation versus those that separated readily; high resistance would suggest genuine prior entanglement, low resistance would support pre-existing partition.',
    'Determines whether Meiji-era violence against combinatory institutions vindicates or undermines the domain-partition reading''s historical claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence, conceptual, 'Whether Meiji-era forced separation confirms or contradicts the claim of pre-existing functional partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 500, 0.14).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.16).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1100, 0.19).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1300, 0.22).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(shin_be_t500, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 500, 0.24).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.27).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1100, 0.3).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1300, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the shinbutsu_coexistence_commitment kernel. domain_partition_reading (this story) authors the lowest ε and suppression of the three, describing tolerant functional coexistence. syncretic_fusion_reading authors a distinct ontological-unification claim (honji suijaku) with its own beneficiary/victim structure centered on Buddhist doctrinal authority over kami. incoherent_bundle_reading authors the arrangement as never coherent, sustained by deliberate ambiguity and institutional power, with correspondingly higher suppression and a collapse narrative tied to Meiji shinbutsu bunri. All three should be read together as competing structural accounts of the same historical kernel, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
