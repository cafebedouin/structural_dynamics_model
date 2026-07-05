% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at as Distributed Cosmic Maintenance (Every Station Sustains Order)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   In ancient Egyptian political theology, Ma'at (order, truth, justice,
 *   cosmic balance) was maintained through practice at every level of
 *   society, from Pharaoh's ritual and administrative acts down to a farmer's
 *   honest dealing with neighbors and a scribe's accurate record-keeping.
 *   Wisdom literature (Instruction of Ptahhotep, Instruction for Merikare)
 *   explicitly instructs officials and commoners alike in station-appropriate
 *   conduct as cosmically significant, and the Weighing of the Heart in the
 *   afterlife judgment applied the same standard to every soul regardless of
 *   earthly rank. This reading treats that universality as structurally real:
 *   Ma'at maintenance is genuinely distributed, with multiple legitimate
 *   interpreters (priesthood, provincial courts, village councils) whose
 *   authority derives from demonstrated conduct rather than delegated royal
 *   grace alone.
 *
 * KEY AGENTS:
 *   - pharaoh: highest-visibility ritual maintainer, but one station among many in this reading (institutional/constrained)
 *   - temple_priesthood: independent ritual authority derived from demonstrated competence (institutional/constrained)
 *   - provincial_officials: local justice and administration, accountable in their own right (powerful/constrained)
 *   - village_councils: local dispute resolution treated as direct instantiation of Ma'at (moderate/constrained)
 *   - artisan_guilds: lowest station, cosmically significant but institutionally powerless (powerless/constrained)
 *   - modern_egyptologists: analytical observers debating whether distributed language is genuine polycentrism or diffused royal ideology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.22).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at as Distributed Cosmic Maintenance (Every Station Sustains Order)").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '8d83f0aa-a9bf-4f92-a629-defb4b017b5b').
narrative_ontology:cs_kernel_codification('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', distributed).
narrative_ontology:cs_authority_grounding('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', practice).
narrative_ontology:cs_interpretation_layer_present('8d83f0aa-a9bf-4f92-a629-defb4b017b5b').
narrative_ontology:cs_reading_relation('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', maat_order_principle__reciprocity_reading, influences).
narrative_ontology:cs_axiom('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', foundational, legitimacy_grounded_in_demonstrated_conduct).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_demonstrated_conduct, holdable).
narrative_ontology:cs_axiom_grounding('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', legitimacy_grounded_in_demonstrated_conduct, conventional).
narrative_ontology:cs_axiom('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', foundational, plural_legitimate_maat_interpreters).
narrative_ontology:cs_axiom_status(plural_legitimate_maat_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', plural_legitimate_maat_interpreters, conventional).
narrative_ontology:cs_reference_frame('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', polycentric_conduct_based_legitimacy).
narrative_ontology:cs_drift_state('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', late_period_textual_attestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d83f0aa-a9bf-4f92-a629-defb4b017b5b', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, provincial_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_class).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, village_councils).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, artisan_guilds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, provincial_officials).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, village_councils).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, cosmic_order_requires_universal_participation).
narrative_ontology:constraint_vindicates(maat_order_principle__distributed_maintenance_reading, station_appropriate_conduct_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs the highest-visibility rituals maintaining Ma'at (temple offerings, coronation renewals, the smiting-of-chaos iconography) but is understood within this reading as one maintainer among many rather than the sole conduit of order. Benefits from legitimacy that is co-produced with, and thus partly dependent on, the conduct of officials, priests, and commoners below him. Cannot unilaterally declare Ma'at satisfied if the broader system of stations is visibly failing.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Administers daily ritual maintenance of Ma'at through offerings and liturgy, deriving institutional authority, land grants, and social standing from being recognized as legitimate interpreters and executors of correct conduct. Their authority in this reading rests on demonstrated ritual competence rather than an exclusive claim to embody order, which gives them standing independent of any single Pharaoh's favor.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, temple_priesthood, agenda_setter).

% Administer justice, tax collection, and irrigation management at the nome level, deriving legitimacy from being seen to maintain Ma'at in their district through fair judgment and effective administration. Bear real accountability: a provincial official who governs unjustly is understood to have broken Ma'at in his own station, independent of Pharaoh's conduct, and can be replaced or censured on that basis.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, provincial_officials, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, provincial_officials, payer).

% Record judgments, maintain administrative order, and transmit wisdom literature instructing every station in proper conduct. Benefit from the doctrine's universality, since it grounds their profession's value in sustaining cosmic order through accurate, honest administration rather than through proximity to the throne alone.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_class, beneficiary,
    moderate, biographical, constrained, national).

% Local elders (kenbet courts) adjudicate disputes and maintain communal order, understanding their own honest judgment as a direct instantiation of Ma'at rather than a delegated fragment of royal authority. This grants local dispute resolution real legitimacy independent of the palace, but also places real accountability on the councils themselves for maintaining fairness.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, village_councils, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, village_councils, payer).

% Craftsmen, farmers, and laborers sustain Ma'at through honest labor, fair dealing, and proper conduct toward family and community, per wisdom texts like the Instruction of Ptahhotep. The doctrine gives even the lowest station genuine cosmic significance and a claim to moral standing that does not depend on royal or priestly mediation, but offers no institutional power to back that claim if a superior station fails to reciprocate.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, artisan_guilds, beneficiary,
    powerless, biographical, constrained, local).

% The weighing-of-the-heart judgment (against the feather of Ma'at) applies the same standard of station-appropriate conduct to every soul regardless of earthly rank, functioning as the doctrine's ultimate evaluative seat and the clearest textual evidence that the framework claims universal, not rank-exclusive, application.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, tomb_and_afterlife_judges, observer,
    analytical, civilizational, analytical, universal).

% Reconstruct the doctrine from wisdom literature, tomb inscriptions, and court records, debating whether distributed-maintenance language in non-royal sources reflects genuine polycentric legitimacy or is itself royal ideology diffused downward to secure compliance at every level without royal enforcement cost.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, modern_egyptologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides every social station — from Pharaoh to farmer — with a shared standard of proper conduct that lets each act correctly without needing case-by-case central direction; local judges, officials, priests, and households each maintain order in their own sphere, and the aggregate of correct local conduct is what sustains cosmic/social order rather than order flowing exclusively downward from the throne.
% TRANSFER_FUNCTION: Primarily reputational and legitimacy-bearing rather than material: correct conduct is transferred into social standing, ritual and judicial authority, and cosmic favor at each station; some material flow exists (temple offerings, tax administered 'justly'), but this reading locates the doctrine's core movement in distributed legitimacy-claims rather than concentrated extraction.
% ABSENT_VOICES: Enslaved persons, foreign captives, and women outside elite households are largely absent from the wisdom-literature record that documents 'proper conduct in one's station' — their stations are defined for them by others, and it is unclear whether the doctrine's promise of universal cosmic participation extended in practice to those with no voice in defining their own station's obligations.
% DISAPPEARANCE_RATIONALE: If the distributed-maintenance framing vanished and only a divine-mandate-through-Pharaoh framing remained, provincial officials, priests, and village councils would lose independent legitimacy grounds for their authority and would become purely delegated agents of the throne — a real institutional rearrangement for the middle and lower administrative strata. Whether ordinary daily conduct (farming, family dealing) would change is more contested: wisdom-literature instructions on honesty and fair dealing plausibly persisted as practical ethics independent of which cosmological frame justified them.
% FOUNDING_PROBLEM: A pre-state or early-state society needed a shared standard of conduct that could hold together administration, justice, and ritual across a large territory without requiring the ruler to personally adjudicate or enforce every local matter — a way to make order self-sustaining at every scale.
% FOUNDING_PROBLEM_CORROBORATION: Wisdom literature (Ptahhotep, Merikare) attributed to officials and addressed to the broadly literate administrative class corroborates the distributed reading from outside the royal court itself. However, most surviving textual production was still produced or sponsored by literate elite strata (scribes, officials, priests) who benefited from the doctrine's legitimation of their own stations; no corroborating source from outside the beneficiary set (e.g., from the illiterate agrarian majority) survives, which modern Egyptologists flag as a significant evidentiary gap.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, contested).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because this reading's structural claim is that legitimacy and obligation run in many directions simultaneously — no single station can extract without its own conduct being independently evaluable, which caps concentrated rent extraction relative to the divine_mandate reading. Suppression is moderate-low (0.28): there is real social pressure to conform to station-appropriate conduct, and real consequences (loss of office, ill repute, cosmic judgment) for failure, but no reading here claims coercive suppression of alternative cosmologies as the doctrine's operative mechanism. Theater ratio is moderate (0.32) and rises slightly over the interval, reflecting a real but partial concern: some of what wisdom literature frames as 'universal cosmic participation' may function partly as ideology that made every station self-policing without requiring royal enforcement infrastructure — a cheap form of order-maintenance for the state precisely because it was distributed as belief rather than administered as law. Accessibility collapse (0.35) and resistance (0.4) are both moderate: the doctrine did not eliminate the possibility of alternative ethical framings (foreign cosmologies, later religious traditions), and its universalist claims met real friction from those whose station was defined without their consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries span nearly every named station (priesthood, officials, scribes, councils, even artisans) because the distributed reading's defining structural feature is that it grants SOME legitimacy and cosmic standing to every level, not concentrated exclusively at the top. No group is named as a victim in this reading specifically because the doctrine's structure — as authored here — does not identify an asymmetric extraction target; costs of maintaining conduct are borne diffusely by everyone required to perform their station correctly, which is closer to genuine coordination cost than extraction. This is the key structural delta from the sibling readings: divine_mandate_reading would show the Pharaoh as concentrated beneficiary with the population as diffuse payer; reciprocity_reading would show extraction if Pharaoh fails to reciprocate. This reading's near-absence of victims is the intended, distinguishing structural signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored contested rather than dead: large-scale, low-enforcement-cost social coordination across administrative, ritual, and judicial spheres is a problem every complex polity continues to face, and the distributed-conduct solution (each station self-polices against a shared, cosmically-backed standard) remains structurally coherent as a solution rather than becoming a dead letter propping up an obsolete arrangement. The mismatch check here (status=contested x disappearance_verdict=contested) does not indicate capture; it indicates genuine live disagreement about whether the doctrine still functioned as intended by the time of its latest textual attestations (Late Period) or had become primarily rhetorical continuity dressed as functioning cosmology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_reading_kernel_disambiguation,
    'Is the distributed-maintenance framing of Ma''at a genuinely independent structural claim, or is it the same divine-mandate structure viewed from the vantage of lower stations, whose ''legitimacy'' is actually fully delegated from and revocable by the Pharaoh?',
    'Comparative textual analysis of whether provincial officials, priests, or village councils could be documented as retaining legitimacy or office against explicit royal disfavor (evidence of genuine independent standing) versus uniformly losing standing upon loss of royal favor (evidence the distributed language is cosmetic delegation).',
    'If independent standing is never documented, this reading collapses structurally into the divine_mandate_reading''s extraction profile (Pharaoh as sole legitimating source) rather than remaining a distinct low-extraction reading; the kernel contest would then be less a live triplet and more one dominant reading with two derivative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_reading_kernel_disambiguation, conceptual, 'Whether distributed_maintenance_reading is structurally independent of divine_mandate_reading or a surface variant of it.').

omega_variable(
    ideology_versus_genuine_polycentrism,
    'Does the wisdom-literature evidence for universal station-appropriate conduct reflect a genuine polycentric distribution of authority, or is it royal/elite ideology that made distributed self-policing (and thus cheap, low-enforcement-cost order) look cosmically mandated to those who bore its costs?',
    'Cross-reference wisdom literature authorship and circulation (was it produced/read primarily by the literate administrative class it flattered?) against archaeological and legal-document evidence of actual local dispute outcomes and their independence from royal intervention.',
    'If ideology-dominant, theater_ratio should be substantially higher than authored and effective extraction should be re-evaluated upward, since ''universal cosmic participation'' would function as a low-cost compliance mechanism rather than genuine distributed legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideology_versus_genuine_polycentrism, empirical, 'Whether distributed-conduct doctrine is genuine polycentric legitimacy or diffused compliance ideology.').

omega_variable(
    excluded_stations_participation_gap,
    'Did the doctrine''s promise of cosmic significance through proper conduct extend in practice to enslaved persons, foreign captives, and women without independent legal standing, or was their ''station'' defined entirely by others with no reciprocal claim available to them?',
    'Examination of legal and funerary evidence (tomb inclusion, afterlife-judgment access, property and testimony rights) for these groups relative to free male administrative-class subjects.',
    'If cosmic participation was effectively unavailable to these groups despite the doctrine''s universalist language, the true beneficiary set is narrower than authored and the extraction profile for excluded groups should be re-evaluated as closer to the reciprocity_reading''s failure mode (obligations owed but not delivered) rather than this reading''s low-extraction distributed-benefit profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_stations_participation_gap, empirical, 'Whether excluded/voiceless populations genuinely shared in distributed cosmic legitimacy or only bore its obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(maat_tr_t0, projected).
narrative_ontology:measurement(maat_tr_t500, maat_order_principle__distributed_maintenance_reading, theater_ratio, 500, 0.27).
narrative_ontology:measurement_basis(maat_tr_t500, projected).
narrative_ontology:measurement(maat_tr_t1000, maat_order_principle__distributed_maintenance_reading, theater_ratio, 1000, 0.29).
narrative_ontology:measurement_basis(maat_tr_t1000, projected).
narrative_ontology:measurement(maat_tr_t1500, maat_order_principle__distributed_maintenance_reading, theater_ratio, 1500, 0.3).
narrative_ontology:measurement_basis(maat_tr_t1500, projected).
narrative_ontology:measurement(maat_tr_t2000, maat_order_principle__distributed_maintenance_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(maat_tr_t2000, projected).
narrative_ontology:measurement(maat_tr_t2500, maat_order_principle__distributed_maintenance_reading, theater_ratio, 2500, 0.32).
narrative_ontology:measurement_basis(maat_tr_t2500, projected).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(maat_be_t0, projected).
narrative_ontology:measurement(maat_be_t500, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 500, 0.19).
narrative_ontology:measurement_basis(maat_be_t500, projected).
narrative_ontology:measurement(maat_be_t1000, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 1000, 0.2).
narrative_ontology:measurement_basis(maat_be_t1000, projected).
narrative_ontology:measurement(maat_be_t1500, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 1500, 0.21).
narrative_ontology:measurement_basis(maat_be_t1500, projected).
narrative_ontology:measurement(maat_be_t2000, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement_basis(maat_be_t2000, projected).
narrative_ontology:measurement(maat_be_t2500, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 2500, 0.22).
narrative_ontology:measurement_basis(maat_be_t2500, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(maat_order_principle__distributed_maintenance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.1).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the maat_order_principle kernel. divine_mandate_reading concentrates legitimacy and extraction potential at the Pharaoh as sole cosmic conduit; reciprocity_reading conditions Pharaoh's legitimacy on delivering justice and resources, producing extraction risk when reciprocity fails; distributed_maintenance_reading (this story) spreads legitimacy and accountability across every station, producing the lowest baseline extraction of the three because no single seat holds an unaccountable claim to embody or guarantee order. The three should be read together, not merged — each has a distinct ε and distinct beneficiary/victim structure appropriate to its own premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
