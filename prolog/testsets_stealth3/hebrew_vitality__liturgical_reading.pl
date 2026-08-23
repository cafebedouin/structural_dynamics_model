% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Unbroken Liturgical Chain as Constitutive Vitality (Liturgical Reading of Hebrew Vitality)
 *   domain: sociolinguistics/language_revitalization/religious_studies
 *
 * SUMMARY:
 *   For some thirteen centuries after the liturgical centers of the Second
 *   Temple fell, Hebrew survived in the synagogue, the study house, and the
 *   scribal workshop rather than the marketplace or the home. This story
 *   models the arrangement by which that survival was organized and claimed
 *   as life: a fixed sacred text copied under strict rules, a standardized
 *   prayer rite deployed identically from Babylonia to the Rhineland, a
 *   school system (heder and yeshiva) that fed boys into the chain young, and
 *   a custodial class — rabbis, cantors, scribes, masoretes — whose authority
 *   rested on the chain's continuity. Under this arrangement the language's
 *   vitality is constituted by unbroken ritual use: so long as each
 *   generation recites, copies, and studies, Hebrew is alive, and the kernel
 *   it occupies is the liturgy itself. Communities funded the chain through
 *   communal chests and tuition; participation was obligatory within the fold
 *   yet wrapped around deep voluntary commitment; and the arrangement
 *   extracted comparatively little — study hours, stipends, deference —
 *   against the continuity, textual unity, and identity it returned. KEY
 *   AGENTS (by structural relationship): - rabbinic_authorities: Primary
 *   custodian and principal beneficiary (institutional / identity_locked) -
 *   praying_congregants: Sustaining beneficiaries (organized /
 *   identity_locked) - liturgical_functionaries: Specialist beneficiaries
 *   (moderate / constrained) - heder_students: Entrant beneficiaries
 *   (powerless / trapped) - women_of_the_community: Excluded voice (moderate
 *   / constrained) - linguistic_historians: Analytical observers (analytical
 *   / analytical)
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Primary custodian and principal beneficiary (institutional / identity_locked) — administers the chain, fixes rite and calendar, accrues authority from its continuity
 *   - praying_congregants: Sustaining beneficiaries (organized / identity_locked) — fund, attend, and reproduce the services; receive continuity and membership in return
 *   - liturgical_functionaries: Specialist beneficiaries (moderate / constrained) — scribes, cantors, masoretes; livelihoods ride the chain they serve
 *   - heder_students: Entrant beneficiaries (powerless / trapped) — children formed into the chain before consent; whether their position is also cost-bearing is held open by omega
 *   - women_of_the_community: Excluded voice (moderate / constrained) — participate via translation, outside the literacy that carries authority
 *   - linguistic_historians: Analytical observers (analytical / analytical) — reconstruct the chain's record from manuscripts and documents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.2).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.28).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Unbroken Liturgical Chain as Constitutive Vitality (Liturgical Reading of Hebrew Vitality)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'a5133f0c-d4a5-411a-913a-118405d09b1e').
narrative_ontology:cs_kernel_codification('a5133f0c-d4a5-411a-913a-118405d09b1e', fixed_text).
narrative_ontology:cs_authority_grounding('a5133f0c-d4a5-411a-913a-118405d09b1e', lineage).
narrative_ontology:cs_interpretation_layer_present('a5133f0c-d4a5-411a-913a-118405d09b1e').
narrative_ontology:cs_reading_relation('a5133f0c-d4a5-411a-913a-118405d09b1e', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5133f0c-d4a5-411a-913a-118405d09b1e', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('a5133f0c-d4a5-411a-913a-118405d09b1e', foundational, liturgical_use_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a5133f0c-d4a5-411a-913a-118405d09b1e', liturgical_use_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('a5133f0c-d4a5-411a-913a-118405d09b1e', foundational, unbroken_transmission_confers_authenticity).
narrative_ontology:cs_axiom_status(unbroken_transmission_confers_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('a5133f0c-d4a5-411a-913a-118405d09b1e', unbroken_transmission_confers_authenticity, theological).
narrative_ontology:cs_reference_frame('a5133f0c-d4a5-411a-913a-118405d09b1e', unbroken_liturgical_occupation).
narrative_ontology:cs_drift_state('a5133f0c-d4a5-411a-913a-118405d09b1e', vernacular_revival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a5133f0c-d4a5-411a-913a-118405d09b1e', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, praying_congregants).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_functionaries).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, heder_students).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, unbroken_masorah_doctrine).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, kedushat_lashon_hakodesh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide what counts as correct recitation, run the courts and schools that train and credential the custodial class, fix the calendar and the rite, and answer questions of practice. Their standing in each community rests on the chain running without a break; they are its public face and its guarantors. Leaving the role would mean ceasing to be what they are — the role and the person have grown together across generations of succession.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Attend, fund, and reproduce the services: they pay communal dues that support teachers and buildings, send sons to school, and recite the fixed texts week after week. What comes back is a place in an unbroken order — the same words their grandparents said, in the same order, joined to communities across the sea. Stepping out entirely would mean leaving the community that constitutes their world, not merely skipping a service.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, praying_congregants, beneficiary,
    organized, generational, identity_locked, local).

% Copy scrolls under exacting rules, chant the public readings, and preserve pronunciation and cantillation. Their livelihoods come from the chain — commissions for scrolls, cantorial posts, teaching posts — and their skills are narrow enough that leaving means retraining from near zero, though Hebrew-letter literacy opened some mercantile doors.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_functionaries, beneficiary,
    moderate, biographical, constrained, regional).

% Enter the schoolroom at four or five and spend their childhood years memorizing script, prayers, and text before they can consent to any of it. What they receive is membership-in-formation: the ability to take their place in the order when grown. They cannot leave; the schoolroom is not theirs to refuse.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, heder_students, beneficiary,
    powerless, immediate, trapped, local).

% Are not obligated in the study track that trains the chain's carriers, and most never learn to read the sacred tongue fluently; they pray in the vernacular from translated editions and hear the service from behind the partition. They keep the households and often the businesses that fund the whole arrangement, and their formal distance from the literate core is a standing fact of its operation.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, women_of_the_community, excluded,
    moderate, biographical, constrained, local).

% Reconstruct the chain's history from manuscripts, colophons, responsa, and the Cairo Geniza; measure where the text stayed fixed and where practice moved; and weigh the arrangement's own account of language survival against the documentary record. They stand outside the obligation and owe the chain nothing.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a scattered, stateless population textually and ritually unified: one standardized scripture, one family of prayer rites, one pronunciation-and-cantillation tradition, and a school network reproducing competent reciters and readers in every settlement, so that a traveler or a letter could move between Babylonia, Kairouan, and Mainz and find the same sacred language in use.
% TRANSFER_FUNCTION: Moves study-years, tuition, communal stipends, and deference from households and congregations to the custodial-scribal class (teachers, scribes, cantors, rabbis); moves the standardized text, the fixed rite, and interpretive rulings back outward to every community in the diaspora.
% ABSENT_VOICES: Women, excluded from the literacy that carries authority, experienced the arrangement through translation and would have described its costs and benefits differently from inside; the children memorizing before consent were never asked; and vernacular-preferring members who quietly let comprehension slip bore a cost nobody tallied. Their absence narrowed whose experience counted as evidence that the language was alive.
% DISAPPEARANCE_RATIONALE: If the chain broke everywhere overnight: no community could conduct its rite, no school could teach from the fixed texts, the custodial class would lose its object and its office at a stroke, and this reading's own account would record the language's death — every arrangement named in this story depends on the recitation continuing.
% FOUNDING_PROBLEM: After the Temple fell (70 CE) and the priesthood's national center dissolved, a small scattered people needed its scripture kept readable, its rite kept uniform, and its interpretive authority kept continuous across empires offering no political protection — the chain was built to carry a sacred language through exile without a state.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Cairo Geniza and geonic responsa independently document communities organizing schools, stipends, and uniform rites around the chain (the genealogy is documented fact, not origin myth), and modern text-critical philology attests the transmitted text's stability across a millennium. No external source attests the normative half of this reading — that the chain constituted vitality rather than admirable preservation — which rests almost wholly inside the benefiting tradition; that asymmetry is recorded here rather than smoothed over.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20 at interval end) because the arrangement's gross transfers are modest — study-hours, stipends, tuition, deference — and are matched by substantial returns (continuity, textual unity, identity, livelihoods for a specialist class); there is no large rent stream. Suppression (0.28) reflects obligation plus communal discipline plus compulsory schooling at the margins, with voluntary devotion carrying most of the load; it is authored as a raw, unscaled structural property — only extractiveness is engine-scaled by directionality and scope. Theater (0.24) prices the growing share of recitation performed without comprehension as the chain widened beyond its dense scholarly core; the specialist spine (scribes, cantors, masoretes) stayed functional throughout, keeping the ratio far from piton territory. Accessibility collapse (0.45): translation aids (Targum, later Yiddish gloss and Tzena Urena) were sanctioned pressure valves inside the frame, and vernacular worship existed outside it; within the frame, once vitality-is-liturgy is accepted, the alternative of letting the language lapse to scholarship alone collapses. Resistance (0.25): the record shows sectarian exits and edge departures (Karaite separation, later Reform vernacularization) rather than organized intra-communal campaigns against the liturgical ideal itself. The claimed type is rope on structural grounds — a genuine, low-overhead coordination achievement whose participants were net beneficiaries — authored independently of these metrics; the engine computes per-seat types from the structural data, and divergence is signal, not error. Measurements share one grid (century marks from Geonic consolidation, c. 750 CE, to the eve of mass vernacularization, c. 1780 CE): extractiveness and theater drift gently upward together as the chain widened, patronage entrenched, and rote grew; suppression_requirement is deliberately not tracked because enforcement intensity was stable across the interval — obligation-based, with no enforcement build-up or decay — so the base_properties scalar carries it. Coordination type is identity_coordination: the chain's dominant job was membership and boundary maintenance, the fixed text serving that end; the identity framing here is load-bearing rather than cover, since the asymmetric extraction it might otherwise be invoked to excuse barely exists.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the custodian seat the arrangement is the tradition itself: administering it and being constituted by it are the same act, and the institutional identity lock (the rabbinate has become its custodial function) makes any challenge to the chain read as attack rather than revision. From the congregant seat the arrangement is inherited obligation fused with devotion — identity-locked exit means the seat's classification inherits the fusion; break the frame (as emancipation eventually did for some) and exit stops being unthinkable. From the entrant seat the same structure arrives without consent: the beneficiary declaration encodes what the child receives, but the omega on formation-cost framing holds open whether that seat computes as bearing a cost. The excluded seat experienced a parallel arrangement through translation — close enough to belong, far enough to lack the literacy that carried authority. Because suppression is authored raw and unscaled while extractiveness is engine-scaled by directionality and scope, the low declared suppression does not guarantee low per-seat effective extraction anywhere an identity-locked target seat emerges; here no seat is declared a target, which is precisely why the entrant-seat question routes to an omega instead of a metric.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared beneficiary clusters near the subsidy end: congregants receive continuity and identity for their dues and devotion; functionaries earn livelihoods from the chain they serve; students receive formation; authorities receive deference, interpretive monopoly, and institutional permanence — the largest single accrual, which is why the receipt surface names that seat. No victims are declared, so no seat derives toward the full-target end from structural data alone; the one candidate cost-bearing position (unconsented child formation) is deliberately left to an omega rather than resolved by a directionality override, because authoring the override would pre-commit the empirical question the omega exists to ask. Scope amplification applies — the chain spans the diaspora, where verification is normally hardest — but its artifacts are unusually verifiable (every scroll checkable letter by letter), which blunts the usual penalty and keeps effective extraction near its low base.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as rope rather than mountain prevents a false-natural-law error: the chain's continuity looks inevitable in retrospect ('Hebrew never died'), but it was manufactured — it required schools, stipends, standardized texts, and constant custodial labor, and nothing about it would persist if everyone stopped performing it. Conversely, refusing the snare label protects the genuine coordination achievement from being read as pure predation: the same structure that accrued authority to custodians also delivered a working sacred register to a stateless diaspora for a millennium. The R5 genealogy keeps the obsolescence question open honestly: the founding problem (post-destruction continuity without sovereignty) has mutated rather than died — sovereignty arrived in 1948 and vernacular life arrived outside the liturgy — so the founding status is authored contested, not dead; the mismatch consumer therefore correctly finds no dead-problem/world-rearranges flag to trip until the parties settle whether transmission-for-transmission's-sake still solves anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This file instantiates only the liturgical_reading of the hebrew_vitality kernel; what would change structurally if a sibling reading (native_daily_reading, hybrid_continuity_reading) were adopted instead?',
    'Cross-file comparison of the three sibling stories over the same standing arrangement: each sibling authors its own epsilon, beneficiary/victim structure, and stakeholder surface; reconciliation occurs at the kernel level, never inside this file.',
    'Adopting native_daily_reading would void this reading''s beneficiary structure (recitation conferring nothing) and re-seat the arrangement''s value on native-speaker transmission; adopting hybrid_continuity_reading would demote this reading''s coordination claim to a necessary-enabler component with a reconstructed second stage. Either way this constraint''s epsilon referent and classification recompute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Kernel-membership and sibling-delta record for the liturgical reading.').

omega_variable(
    vitality_definition_dispute_location,
    'Where exactly do the three readings disagree — is the dispute located in the definition of language vitality (the constitution question) rather than in the empirical record of preservation?',
    'Element-location analysis: all three readings agree the ritual chain operated continuously and agree preservation occurred; they differ solely on what counts as the language being alive — liturgical use alone (this file), native generation alone, or substrate-plus-reconstruction.',
    'Because the disagreement is definitional, no usage-frequency dataset settles it; adjudication requires an agreed vitality criterion, which would collapse two of the three readings into footnotes and reclassify the survivors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_dispute_location, conceptual, 'The readings'' disagreement sits at the constitution criterion, not at the historical record.').

omega_variable(
    rote_comprehension_function_question,
    'Was congregational recitation without comprehension functional transmission (the chain doing its job through its specialist spine) or a performative shell around a shrinking living core?',
    'Historical pedagogical and literacy evidence: comprehension proxies, uptake of translation aids (Targum, Tzena Urena), and the density of genuinely competent lay readers per congregation per century.',
    'If the performative share dominated late-period recitation, theater_ratio climbs toward piton territory and this reading''s claim weakens from within; if specialist density sufficed, the chain remained functional and the reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rote_comprehension_function_question, empirical, 'Functional-versus-performative composition of the recitation chain over time.').

omega_variable(
    child_formation_cost_framing,
    'Does compulsory childhood formation (years of memorization before consent, borne unequally) constitute a cost-bearing position that would add a victim set and pull the arrangement toward tangled_rope — or is it, within the tradition''s own accounting, formation received rather than extraction endured?',
    'Frame comparison: the tradition''s internal accounting (formation as obligation and gift) set against external welfare accounting (unconsented compulsory labor), with empirical inputs on time burdens and age of entry; the classification outcome depends on which frame governs.',
    'Under the external-welfare frame a victim set appears (heder_students as payers), effective extraction rises for that seat, and the arrangement reclassifies toward tangled_rope; under the internal frame the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(child_formation_cost_framing, conceptual, 'Whether child formation reads as cost-bearing depends on whose accounting frame is adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 1030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t200, hebrew_vitality__liturgical_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t200, observed).
narrative_ontology:measurement(hebr_tr_t400, hebrew_vitality__liturgical_reading, theater_ratio, 400, 0.14).
narrative_ontology:measurement_basis(hebr_tr_t400, observed).
narrative_ontology:measurement(hebr_tr_t600, hebrew_vitality__liturgical_reading, theater_ratio, 600, 0.17).
narrative_ontology:measurement_basis(hebr_tr_t600, observed).
narrative_ontology:measurement(hebr_tr_t800, hebrew_vitality__liturgical_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t800, observed).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_vitality__liturgical_reading, theater_ratio, 1000, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t1000, observed).
narrative_ontology:measurement(hebr_tr_t1030, hebrew_vitality__liturgical_reading, theater_ratio, 1030, 0.24).
narrative_ontology:measurement_basis(hebr_tr_t1030, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t200, hebrew_vitality__liturgical_reading, base_extractiveness, 200, 0.13).
narrative_ontology:measurement_basis(hebr_be_t200, observed).
narrative_ontology:measurement(hebr_be_t400, hebrew_vitality__liturgical_reading, base_extractiveness, 400, 0.14).
narrative_ontology:measurement_basis(hebr_be_t400, observed).
narrative_ontology:measurement(hebr_be_t600, hebrew_vitality__liturgical_reading, base_extractiveness, 600, 0.16).
narrative_ontology:measurement_basis(hebr_be_t600, observed).
narrative_ontology:measurement(hebr_be_t800, hebrew_vitality__liturgical_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement_basis(hebr_be_t800, observed).
narrative_ontology:measurement(hebr_be_t1000, hebrew_vitality__liturgical_reading, base_extractiveness, 1000, 0.19).
narrative_ontology:measurement_basis(hebr_be_t1000, observed).
narrative_ontology:measurement(hebr_be_t1030, hebrew_vitality__liturgical_reading, base_extractiveness, 1030, 0.2).
narrative_ontology:measurement_basis(hebr_be_t1030, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial question 'did Hebrew stay alive?' bundles three structurally distinct claims — liturgy-constitutes-life (this file), natives-only, and substrate-plus-reconstruction. Each reading is authored as its own constraint with its own epsilon and stakeholder surface over the same standing historical arrangement, linked here via affects_constraints; epsilon divergence across siblings is the measured quantity, not an error to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
