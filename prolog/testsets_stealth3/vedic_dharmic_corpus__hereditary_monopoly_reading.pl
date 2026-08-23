% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Ritual-Authority Reading of the Vedic-Dharmic Corpus
 *   domain: religious/social/interpretive-legitimacy
 *
 * SUMMARY:
 *   This story instantiates one reading of a stabilized scriptural corpus:
 *   the arrangement under which the right to officiate at sacrifice, to teach
 *   the scriptures, and to adjudicate ritual questions is fixed by descent
 *   into recognized lineages, with the fourfold social order treated as part
 *   of the revealed order rather than as a revisable institution. The
 *   arrangement runs on a large institutional base — endowed temples,
 *   recitation schools attached to lineages, a purity regime governing
 *   contact, food, and space, and a ritual economy in which lay communities
 *   fund specialist officiants for every life transition and calendrical
 *   rite. Its benefits concentrate on the priestly class, with ruling patrons
 *   purchasing legitimation through the same channel; its costs concentrate
 *   on laboring castes, outcaste communities, and women, who are barred from
 *   the training that would qualify them to claim any part of the authority
 *   in question and who are bound to their positions by inheritance.
 *   Enforcement is continuous and institutional rather than episodic:
 *   precinct policing, denial of instruction, sanction against transgression,
 *   and doctrinal framing that locates dissent outside the moral order
 *   altogether. Claimed type and metrics are authored independently: I claim
 *   tangled_rope because a real coordination good (multi-generational corpus
 *   transmission, determinate officiant succession, standardized rite) is
 *   bundled, through the same gates, with sharply asymmetric burden-bearing;
 *   the metrics record the operation as the historical record shows it, and
 *   any divergence between claim and computed type is the datum. The ε
 *   referent is the standing hereditary-authority arrangement itself, valued
 *   through this reading's operative criteria — what it counts as owed, owed
 *   to whom, and forbidden — landing at 0.66, the manifest's expected band
 *   refined by the enforcement and receipt evidence summarized below. KEY
 *   AGENTS (by structural relationship): - brahmin_priestly_class:
 *   Agenda-setter and primary beneficiary (institutional/generational,
 *   identity_locked) — administers interpretation and the ritual schedule;
 *   collects fees and endowment income - kshatriya_ruling_elites:
 *   Patron-beneficiary (powerful/biographical, constrained) — finances the
 *   arrangement in exchange for legitimation of rule -
 *   temple_institution_network: Institutional enforcement arm
 *   (institutional/generational, constrained) — controls sacred space and
 *   ritual employment - laboring_shudra_castes: Primary target
 *   (powerless/generational, trapped) — bears service obligations and ritual
 *   exclusion - dalit_outcaste_communities: Most burdened target
 *   (powerless/generational, trapped) — denied precinct entry, assigned
 *   polluting labor - women_denied_ritual_standing: Cross-cutting target
 *   (powerless/biographical, trapped) — excluded from initiation and
 *   independent ritual agency - ascetic_devotional_movements: Excluded
 *   challenger (organized/generational, constrained) — contests
 *   birth-qualification from outside the adjudicating councils -
 *   historians_of_indian_religion: Analytical observer
 *   (analytical/generational, analytical) — sees the full structure
 *
 * KEY AGENTS:
 *   - - brahmin_priestly_class: Agenda-setter and primary beneficiary (institutional/generational, identity_locked) — administers interpretation and the ritual schedule; collects fees and endowment income
 *   - - kshatriya_ruling_elites: Patron-beneficiary (powerful/biographical, constrained) — finances the arrangement in exchange for legitimation of rule
 *   - - temple_institution_network: Institutional enforcement arm (institutional/generational, constrained) — controls sacred space and ritual employment
 *   - - laboring_shudra_castes: Primary target (powerless/generational, trapped) — bears service obligations and ritual exclusion
 *   - - dalit_outcaste_communities: Most burdened target (powerless/generational, trapped) — denied precinct entry, assigned polluting labor
 *   - - women_denied_ritual_standing: Cross-cutting target (powerless/biographical, trapped) — excluded from initiation and independent ritual agency
 *   - - ascetic_devotional_movements: Excluded challenger (organized/generational, constrained) — contests birth-qualification from outside the adjudicating councils
 *   - - historians_of_indian_religion: Analytical observer (analytical/generational, analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.72).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Ritual-Authority Reading of the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social/interpretive-legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'a88344eb-c3c3-4426-bba9-fb546dd81f59').
narrative_ontology:cs_kernel_codification('a88344eb-c3c3-4426-bba9-fb546dd81f59', fixed_text).
narrative_ontology:cs_authority_grounding('a88344eb-c3c3-4426-bba9-fb546dd81f59', lineage).
narrative_ontology:cs_interpretation_layer_present('a88344eb-c3c3-4426-bba9-fb546dd81f59').
narrative_ontology:cs_reading_relation('a88344eb-c3c3-4426-bba9-fb546dd81f59', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('a88344eb-c3c3-4426-bba9-fb546dd81f59', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('a88344eb-c3c3-4426-bba9-fb546dd81f59', foundational, ritual_authority_fixed_by_birth).
narrative_ontology:cs_axiom_status(ritual_authority_fixed_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('a88344eb-c3c3-4426-bba9-fb546dd81f59', ritual_authority_fixed_by_birth, theological).
narrative_ontology:cs_axiom('a88344eb-c3c3-4426-bba9-fb546dd81f59', foundational, varna_hierarchy_divinely_prescribed).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('a88344eb-c3c3-4426-bba9-fb546dd81f59', varna_hierarchy_divinely_prescribed, theological).
narrative_ontology:cs_axiom('a88344eb-c3c3-4426-bba9-fb546dd81f59', secondary, corpus_fidelity_requires_lineage_discipline).
narrative_ontology:cs_axiom_status(corpus_fidelity_requires_lineage_discipline, holdable).
narrative_ontology:cs_axiom_grounding('a88344eb-c3c3-4426-bba9-fb546dd81f59', corpus_fidelity_requires_lineage_discipline, instrumental).
narrative_ontology:cs_reference_frame('a88344eb-c3c3-4426-bba9-fb546dd81f59', birth_ordained_custodianship).
narrative_ontology:cs_drift_state('a88344eb-c3c3-4426-bba9-fb546dd81f59', modern_equality_jurisprudence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a88344eb-c3c3-4426-bba9-fb546dd81f59', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, laboring_shudra_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_denied_ritual_standing).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, apaurusheyatva_eternal_revelation_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, karma_rebirth_theodicy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Born into lineages charged with memorizing and transmitting the Vedic corpus; alone entitled to officiate at sacrifices, to teach the scriptures, and to pronounce on ritual questions. Collects ceremonial fees, offerings, and shares of temple endowment income. Bound in turn by demanding purity disciplines and lifelong study; stepping outside the priestly calling forfeits the standing the identity provides. Admits no route to ritual office except descent.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, beneficiary).

% Hold military and governmental power and patronize the priestly class, funding sacrifices, temples, and scholars with land grants and treasure in exchange for consecration, dynastic genealogy, and sanction of their command. Their legitimacy travels through the arrangement they finance; abandoning it means searching for rival sources of sacred legitimation at real cost.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, kshatriya_ruling_elites, beneficiary,
    powerful, biographical, constrained, regional).

% Administers endowed temples: schedules festivals, employs ritual specialists, guards precincts, and polices who may enter and worship. Accumulates land, stores, and treasury; disburses emoluments to the officiating lineages. Enforces purity boundaries at its gates.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_institution_network, agenda_setter,
    institutional, generational, constrained, continental).

% Farm, build, and serve under hereditary occupational assignments. Barred from Vedic study and from officiating roles; may hear vernacular epic teaching but depend on priestly intermediaries for every life-cycle rite. Position passes from parent to child; leaving means losing caste community, occupation, and marriage network simultaneously.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, laboring_shudra_castes, payer,
    powerless, generational, trapped, regional).

% Placed outside the settled village order: assigned polluting but indispensable labor, segregated in housing, and denied entry to temple precincts and often to wells and schools. Contact itself is treated as contaminating; sanctions fall on them for proximity they cannot avoid.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_outcaste_communities, payer,
    powerless, generational, trapped, regional).

% Across castes, girls receive no initiatory qualification for Vedic learning and so stand outside independent ritual capacity; their religious agency runs through fathers and husbands, with widowhood restrictions falling hardest. Vernacular devotional practice is open to them; authorized teaching and officiation are not.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_denied_ritual_standing, payer,
    powerless, biographical, trapped, regional).

% Renunciant and popular devotional teachers draw followers across caste lines, sing vernacular critiques of birth-qualification, and install leaders from laboring and outcaste backgrounds. They preach along trade and pilgrimage routes rather than sitting on the councils where scripture is adjudicated; their teachings are periodically condemned, absorbed, or honored only after the teacher's death.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, ascetic_devotional_movements, excluded,
    organized, generational, constrained, continental).

% Study inscriptions, manuscripts, court records, and legal and missionary archives; reconstruct how the arrangement financed itself, how enforcement varied by region and century, and how challenges were handled. Hold no stake in officiating rights.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, historians_of_indian_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits an enormous memorized liturgical corpus across generations without writing-centered backup; provides determinate succession for ritual officiants (no contest over who may preside); standardizes rite and festival calendars across dispersed communities; staffs every life-transition and agricultural-cycle ceremony.
% TRANSFER_FUNCTION: Moves material support (ceremonial fees, grain shares, land-grant and endowment income), labor service, and deference from lay communities — disproportionately laboring and outcaste castes — to the priestly class and its temple institutions; moves interpretive authority itself exclusively to hereditary holders, and moves legitimation from the priestly class to ruling patrons who finance it.
% ABSENT_VOICES: Laboring castes, outcaste communities, and women hold no seat in the councils where orthodoxy is adjudicated; ascetic and devotional teachers address crowds outside the canonical fora, so their objections register as disorder to be managed rather than as premises to weigh. Their absence from adjudication is what lets unanimity about divine ordination appear consensual.
% DISAPPEARANCE_RATIONALE: Overnight removal would throw officiation open to trained aspirants regardless of descent, reorganize corpus transmission around open academies, redistribute temple endowment income, strip marriage and residence rules of sacral warrant, and dissolve the purity geography of villages and towns within a generation — the stratified order would rebuild itself around different legitimacy sources.
% FOUNDING_PROBLEM: Secure faithful transmission of a vast oral corpus and guarantee every community a universally recognized officiant in a world without printing or a centralized church; the lineage mechanism was built so that competence and entitlement to transmit would travel together by descent.
% FOUNDING_PROBLEM_CORROBORATION: The tradition's own recitation lineages attest continuing necessity. Corroboration from outside the benefiting parties: epigraphic and legal-historical records documenting how endowments and caste-service obligations actually operated; philological and print-era scholarship showing the transmission function is now carried by open academies and published texts; testimony of devotional and anti-caste movements that spiritual access never required birth qualification. No attestation independent of the priestly class supports the claim that descent remains necessary.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 because the transfers are large, compulsory in practice, and decoupled from the cost of the services rendered: fees and endowment income flow to lineages whose qualifying asset is birth, while the populations bearing the heaviest burdens receive no reciprocal access to the authority that prices their exclusion. Suppression (0.72) is a raw structural input, deliberately unscaled — the engine scales only extractiveness by directionality and scope — and it records the enforcement machinery itself: denial of instruction, precinct policing, purity sanction, and doctrinal disqualification of dissent. Accessibility collapse sits at 0.55: alternatives never disappeared (renunciant orders, vernacular devotional currents, heterodox schools persisted at the margins), but inside villages and courts the practical exits — education, occupational mobility, conversion without social death — stayed closed for nearly everyone for most of the interval. Resistance (0.6) is high and recurrent: the record shows repeated waves of challenge from below and from renunciants, met alternately with condemnation and absorption. The measurement series run on one shared grid (one unit ≈ 50 years; t0 ≈ 600 CE; t24 ≈ 1800 CE), with every tracked metric authored at every point. All three series dip around t≈1200 and t≈1500, corresponding to major popular-devotional surges that forced partial openings — wider vernacular teaching, veneration of low-born teachers — followed by re-consolidation through scholastic synthesis and selective posthumous canonization. The oscillation is not noise: periodic concession bought down resistance cheaply, after which enforcement re-tightened — an intermittent-reinforcement cycle in which the swing itself helps hold the arrangement. Identity-lock operates differently by seat: for the priestly class it is institutional (the lineage has become its function; leaving office forfeits the identity's entire payoff), while for the subordinated seats it is relational and doctrinal (community, marriageability, and the karma-framing of one's place are all constituted inside the arrangement). Coalition among the burden-bearing groups was structurally impeded: segmentary kin organization separated interests village by village, and the purity code made cross-caste assembly itself sanctionable — a fragmentation the arrangement had every interest in maintaining.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute divergent classifications from identical facts. From inside the priestly seat the arrangement reads as fulfillment of obligation: fees are reciprocity, exclusion is order, hierarchy is prescription — extraction is invisible because the categories that would register it are the ones the seat itself administers. From the laboring-caste and outcaste seats the same facts read as exclusion priced in perpetuity: they pay for rites they may not perform, help endow precincts they may not enter, and inherit positions they did not choose. The ruling-patron seat reads a purchased service — costly, but exchanged for consecration it cannot otherwise obtain. The engine computes these per-seat divergences from the declared roles, powers, and exits; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: brahmin_priestly_class and kshatriya_ruling_elites sit at the beneficiary end; laboring_shudra_castes, dalit_outcaste_communities, and women_denied_ritual_standing sit at the target end, with trapped exits pushing them toward full-target weighting and amplifying effective burden; temple_institution_network inherits a beneficiary-side placement as the collecting-and-administering arm. Two overrides correct derivations the declarations cannot reach. First, kshatriya_ruling_elites (powerful): the bare beneficiary declaration would push d toward 0.0, but the seat transfers substantial land and treasure to purchase consecration — its position is nearer a costly exchange than a subsidy, so d is overridden to 0.35. Second, ascetic_devotional_movements (organized): they hold no slot in the beneficiary or victim arrays, so structural derivation has no signal for them and the power-atom fallback would place them near symmetry; in fact they are among the arrangement's principal opponents, absorbing suppression directly, so d is overridden to 0.75. Suppression itself stays unscaled throughout; only extractiveness is amplified by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — keeping a vast memorized corpus intact across centuries and guaranteeing every community a recognized officiant — were real, and the lineage mechanism addressed them under pre-print conditions. Both problems have since been answered by other means (printing, open academies, non-hereditary clerical training elsewhere), which is why founding_problem_status is authored 'contested': the tradition attests continuing necessity, the external record attests the mechanism's redundancy while the needs themselves persist. Classifying the arrangement as tangled_rope rather than snare keeps the genuine service visible and blocks a wholesale reading of the structure as pure predation; refusing the rope label honors the documented burden-bearing. The forward risk is piton drift: as literal transmission-fidelity ceases to bind anyone, the residue is status maintenance and ceremonial performance — visible in the rising theater_ratio — administered by a class that could revise the arrangement only at the cost of dissolving the office-holding identity that constitutes it. That cost-asymmetry is recorded on the receipt surface: gains accrue to a named seat, and fixing is prohibitive for the only seats with the power to fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the vedic_dharmic_corpus kernel fix any single reading, or is it genuinely multi-stable such that the divine-ordination claim reflects control over interpretation rather than textual compulsion?',
    'Compare which textual strata (recited collections, ritual explanation layers, rule codes, later legal digests, commentarial tradition) each reading of the corpus treats as controlling, and test whether the ordination claim survives restriction to the earliest strata it cites.',
    'If the kernel is multi-stable, the arrangement''s warrant rests on custody of interpretation rather than on the texts themselves, strengthening the extraction reading of the birth-gate; if some stratum decisively fixes the reading, part of the measured burden is doctrinally compelled rather than strategically maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether the corpus fixes this reading or merely licenses it under interpretive custody.').

omega_variable(
    birth_qualification_separability,
    'Is descent into a lineage structurally necessary for faithful corpus transmission and legitimate officiation, or is the birth-gate separable from the transmission function it justifies?',
    'Compare fidelity and continuity outcomes across closed lineages and open teaching institutions (print-era academies, university philology, non-hereditary ordination in comparable scriptural traditions).',
    'If separable, the gate is a rent-collection device riding on a real function and the arrangement trends toward the pure-extraction pole; if inseparable, a share of the measured burden is coordination cost and the hybrid classification firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_qualification_separability, empirical, 'Whether the hereditary gate is separable from the transmission service.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression that holds the subordinate seats in place structural (denial of instruction, precinct policing, economic severance) or internalized (karma-framing of one''s place, purity habitus, fear of community loss)?',
    'Track deference and purity-observance trajectories in communities after conversion, migration, or constitutional emancipation: persistence of the patterns after barrier removal indicates an internalized share.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlives formal change; successor arrangements must then price the carried residue rather than crediting emancipation with full relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    enforcement_attribution,
    'How much of the enforcement burden traces to this reading''s doctrinal content specifically, versus the surrounding agrarian-political economy (land tenure, patronage, state formation) that almost any legitimating ideology could have served?',
    'Regional-comparative analysis correlating variation in doctrinal stringency with variation in land relations and patronage structure while holding the textual corpus constant.',
    'If enforcement tracks political economy, removing the reading alone would not dismantle the burdens and sibling readings inherit the enforcement problem; if it tracks doctrine, the reading itself is the load-bearing element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_attribution, empirical, 'Doctrine-driven versus political-economy-driven enforcement.').

omega_variable(
    epsilon_seat_indexing,
    'The ε authored here is indexed to this reading''s own operative criteria over the standing arrangement; would indexing through a rival reading''s criteria yield a different value over the same referent?',
    'Cross-file comparison of authored ε across the kernel family''s stories over the shared referent: convergence bounds topic-level claims; divergence is reading-index variance to be reported, not reconciled.',
    'Prevents misreading this ε as a topic-level constant; comparability across the kernel family must run through the shared referent, not through shared values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_seat_indexing, conceptual, 'Reading-indexed ε over a fixed referent; cross-reading comparability limit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vedi_tr_t0, observed).
narrative_ontology:measurement(vedi_tr_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(vedi_tr_t3, observed).
narrative_ontology:measurement(vedi_tr_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(vedi_tr_t6, observed).
narrative_ontology:measurement(vedi_tr_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement_basis(vedi_tr_t9, observed).
narrative_ontology:measurement(vedi_tr_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(vedi_tr_t12, observed).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(vedi_tr_t15, observed).
narrative_ontology:measurement(vedi_tr_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(vedi_tr_t18, observed).
narrative_ontology:measurement(vedi_tr_t21, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement_basis(vedi_tr_t21, observed).
narrative_ontology:measurement(vedi_tr_t24, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(vedi_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vedi_be_t0, observed).
narrative_ontology:measurement(vedi_be_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 3, 0.46).
narrative_ontology:measurement_basis(vedi_be_t3, observed).
narrative_ontology:measurement(vedi_be_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(vedi_be_t6, observed).
narrative_ontology:measurement(vedi_be_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 9, 0.54).
narrative_ontology:measurement_basis(vedi_be_t9, observed).
narrative_ontology:measurement(vedi_be_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement_basis(vedi_be_t12, observed).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(vedi_be_t15, observed).
narrative_ontology:measurement(vedi_be_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(vedi_be_t18, observed).
narrative_ontology:measurement(vedi_be_t21, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 21, 0.59).
narrative_ontology:measurement_basis(vedi_be_t21, observed).
narrative_ontology:measurement(vedi_be_t24, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(vedi_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(vedi_su_t0, observed).
narrative_ontology:measurement(vedi_su_t3, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement_basis(vedi_su_t3, observed).
narrative_ontology:measurement(vedi_su_t6, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(vedi_su_t6, observed).
narrative_ontology:measurement(vedi_su_t9, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 9, 0.6).
narrative_ontology:measurement_basis(vedi_su_t9, observed).
narrative_ontology:measurement(vedi_su_t12, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(vedi_su_t12, observed).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(vedi_su_t15, observed).
narrative_ontology:measurement(vedi_su_t18, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(vedi_su_t18, observed).
narrative_ontology:measurement(vedi_su_t21, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 21, 0.66).
narrative_ontology:measurement_basis(vedi_su_t21, observed).
narrative_ontology:measurement(vedi_su_t24, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(vedi_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus__reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the vedic_dharmic_corpus kernel: the colloquial label 'the scripturally sanctioned hierarchical order' covers three structurally distinct arrangements. This member carries the highest ε of the family (~0.66): a real transmission function gated by descent and backed by institutional enforcement. The bhakti sibling reroutes authority to devotion and lowers ε; the reformist sibling attacks the ordination premise directly. Influence runs outward from this reading: it sets the legitimacy conditions both siblings define themselves against, so its edges point to both. A further decomposition candidate noted for future authorship: the corpus-transmission function (information_standard) is separable from the boundary-maintenance function (identity_coordination) that dominates this story's coordination typing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, powerful, 0.35).
constraint_indexing:directionality_override(vedic_dharmic_corpus__hereditary_monopoly_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
