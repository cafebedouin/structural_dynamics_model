% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__young_earth_literal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__young_earth_literal, []).

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
 *   constraint_id: genesis_creation_cosmology__young_earth_literal
 *   human_readable: Young-Earth Literal-Day Doctrine as Enforced Interpretive Regime
 *   domain: religious/theological/philosophy_of_science
 *
 * SUMMARY:
 *   Within communities that hold the young-earth-literal reading, the
 *   doctrine operates as a binding interpretive regime: confessional
 *   statements require affirmation of six literal 24-hour days of recent
 *   creation (~6000-10000 years ago), ministries produce curricula and media
 *   that subordinate empirical consensus to the plain sense of the text, and
 *   departure from the reading carries disciplinary and relational cost. A
 *   genuine coordination function (identity, certainty, transmission of
 *   scriptural authority) rides on the same structure that imposes asymmetric
 *   costs: scientific consensus is misrepresented inside the perimeter,
 *   students receive distorted pedagogy where the doctrine shapes standards,
 *   and dissenters pay in standing and relationships. KEY AGENTS (by
 *   structural relationship): - creationist_ministries: agenda-setting
 *   producer/enforcer (organized/identity_locked) — runs the doctrine's
 *   media, curricula, and venues; collects its revenue -
 *   denominational_authorities: agenda-setting enforcer
 *   (institutional/identity_locked) — guards confessional boundaries,
 *   disciplines dissent - lay_believers: dual-positioned rank-and-file
 *   (moderate/identity_locked) — funded beneficiaries who also bear indirect
 *   costs - scientific_community: primary epistemic target
 *   (institutional/mobile) — its findings are misrepresented where the
 *   doctrine is enforced - public_school_students: captive audience
 *   (powerless/trapped) — bear pedagogy costs they cannot opt out of -
 *   biology_teachers: constrained intermediaries (moderate/constrained) —
 *   self-censor under community pressure - doctrinal_dissenters: internal
 *   targets (moderate/identity_locked) — pay relational and standing costs
 *   for accepting science - theistic_evolution_advocates: excluded advocates
 *   (organized/mobile) — promote sibling readings from outside the perimeter
 *   - courts_and_legislatures: analytical observer (institutional/analytical)
 *   — bound the legal reach of enforcement CONSTRAINT FAMILY NOTE
 *   (epsilon-invariance): 'what Genesis says about creation' decomposes into
 *   three structurally distinct readings of one kernel. This file
 *   instantiates young_earth_literal only; theistic_evolution and
 *   literary_framework are separate stories linked via
 *   network.affects_constraints. This reading carries the family's highest
 *   epsilon because it alone subordinates empirical method to textual
 *   authority and therefore requires active suppression of rival readings and
 *   pedagogy; the siblings shed the victim set and the enforcement burden.
 *   The epsilon referent here is the enforced arrangement as it actually
 *   operates — never the arrangements the sibling readings would put in its
 *   place.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, 0.74).
domain_priors:suppression_score(genesis_creation_cosmology__young_earth_literal, 0.78).
domain_priors:theater_ratio(genesis_creation_cosmology__young_earth_literal, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, extractiveness, 0.74).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__young_earth_literal, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__young_earth_literal, "Young-Earth Literal-Day Doctrine as Enforced Interpretive Regime").
narrative_ontology:topic_domain(genesis_creation_cosmology__young_earth_literal, "religious/theological/philosophy_of_science").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__young_earth_literal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__young_earth_literal, 'b7e5eedb-1bca-4da7-91f3-2f4db042e36b').
narrative_ontology:cs_kernel_codification('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', fixed_text).
narrative_ontology:cs_authority_grounding('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', lineage).
narrative_ontology:cs_interpretation_layer_present('b7e5eedb-1bca-4da7-91f3-2f4db042e36b').
narrative_ontology:cs_reading_relation('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', genesis_creation_cosmology__theistic_evolution, forecloses).
narrative_ontology:cs_reading_relation('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', genesis_creation_cosmology__literary_framework, forecloses).
narrative_ontology:cs_axiom('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', foundational, recent_literal_creation_history).
narrative_ontology:cs_axiom_status(recent_literal_creation_history, holdable).
narrative_ontology:cs_axiom_grounding('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', recent_literal_creation_history, empirically_contingent).
narrative_ontology:cs_axiom('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', foundational, plain_sense_supremacy_over_empiricism).
narrative_ontology:cs_axiom_status(plain_sense_supremacy_over_empiricism, holdable).
narrative_ontology:cs_axiom_grounding('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', plain_sense_supremacy_over_empiricism, theological).
narrative_ontology:cs_reference_frame('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', plain_sense_literal_hexaemeron).
narrative_ontology:cs_drift_state('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', contemporary_post_kitzmiller, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7e5eedb-1bca-4da7-91f3-2f4db042e36b', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, creationist_ministries).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, denominational_authorities).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, lay_believers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, scientific_community).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, public_school_students).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, biology_teachers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, doctrinal_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, lay_believers).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, recent_special_creation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the doctrine's production and enforcement infrastructure: publish curricula, run museums and media outlets, train speakers, and maintain doctrinal boundaries through speaker vetting and platform control. Revenue arrives through donations, admissions, and book and curriculum sales tied directly to the doctrine's continuation. Leadership careers, donor bases, and institutional missions are fused with the doctrine; pivoting away would dissolve the organizations' reason for existence.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, creationist_ministries, agenda_setter,
    organized, generational, identity_locked, global).

% Maintain confessional statements that require affirmation of recent literal creation for ordination, membership in good standing, and institutional affiliation. Discipline clergy and educators who depart from the reading, and coordinate with ministries on educational materials. Their authority and cohesion rest on guarding the confession; revising it risks schism.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, denominational_authorities, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__young_earth_literal, denominational_authorities, beneficiary).

% Affirm the doctrine as part of communal identity: it supplies certainty about origins, a shared story, and a boundary against secular culture. They fund the apparatus through tithes and purchases, enroll children in affected schools and curricula, and encounter mainstream science mainly through apologetic paraphrase. Leaving the community would cost family ties, friendships, and self-concept, so doubt tends to be managed privately rather than acted on.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, lay_believers, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__young_earth_literal, lay_believers, payer).

% Produce the radiometric, genomic, geological, and cosmological findings that contradict the doctrine's factual claims. Inside communities that enforce the doctrine their work appears only in hostile summary, and their consensus is misrepresented in curricula and public materials. Outside those communities they operate normally — journals, universities, and funding are unaffected — so the costs they bear concentrate wherever the doctrine is enforced.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, scientific_community, payer,
    institutional, civilizational, mobile, global).

% In districts and states where the doctrine shapes standards, textbook adoption, or classroom speech, they receive evolution abbreviated, hedged, or framed as disputed, and may encounter creation-framed material. They cannot choose their schools, and the quality of their preparation for college biology varies with local politics they do not control.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, public_school_students, payer,
    powerless, biographical, trapped, national).

% Navigate community pressure, board policies, and textbook limitations when teaching evolution; many self-censor coverage to avoid conflict. Changing districts or leaving the profession carries real costs, and their professional judgment sits between scientific training and local expectations.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, biology_teachers, payer,
    moderate, biographical, constrained, national).

% Members of enforcing communities who accept mainstream science. They face counseling, exclusion from teaching or leadership roles, damaged standing, and family strain; some leave at high relational cost, others conform outwardly while doubting privately. Their objections rarely reach the forums where the doctrine is maintained.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, doctrinal_dissenters, payer,
    moderate, biographical, identity_locked, regional).

% Organizations and scholars promoting readings in which Genesis communicates theological truth through non-literal forms compatible with evolutionary science. Enforcing bodies condemn their work as compromise; they publish, convene, and advocate from outside the confessional perimeter, with limited access to the audiences the doctrine's institutions control.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, theistic_evolution_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate and legislate the pedagogy disputes: anti-evolution statutes, balanced-treatment acts, and standards battles have repeatedly reached the courts, which have struck down several enforcement vehicles. They set the legal outer boundary of what the doctrine's institutions may require of public education.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__young_earth_literal, creationist_ministries).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__young_earth_literal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community around a single authoritative reading of Genesis: shared identity, epistemic certainty about origins, intergenerational transmission of scriptural authority, and a maintained boundary between the community and secular intellectual culture.
% TRANSFER_FUNCTION: Moves money (donations, tuition, admissions, curriculum sales) from lay believers to ministry institutions; moves epistemic authority from empirical science to textual interpretation within the enforcing perimeter; moves status toward members who affirm the doctrine and away from those who doubt it.
% ABSENT_VOICES: Scientists and science educators are structurally absent from the communities where the doctrine is enforced — their testimony enters only through hostile paraphrase in apologetic literature. Doubting members rarely speak where discipline makes silence rational. Advocates of alternative readings are present in the wider culture but excluded from the confessional conversation where the doctrine is maintained.
% DISAPPEARANCE_RATIONALE: If the enforced doctrine vanished overnight, ministry institutions would lose their revenue base and organizing purpose, denominational identities built on the confession would fracture, the recurring public-school pedagogy disputes would lose their central driver, and millions of believers would need to reconstruct their account of origins and textual authority. Science pedagogy in affected districts would normalize toward the mainstream consensus within a generation.
% FOUNDING_PROBLEM: Consolidated during the fundamentalist-modernist controversy (with older roots in Ussher-era chronologies): how to preserve the plain-sense authority of Genesis when geology and evolutionary biology appeared to contradict a literal reading of the creation narrative.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion, writing from outside the benefiting parties, document the doctrine's consolidation as an authority-defense movement in the fundamentalist-modernist controversy. Court records (Epperson, Edwards v. Aguillard, Kitzmiller v. Dover) corroborate the pedagogy-control objective, and scientific societies' public statements corroborate the misrepresentation and suppression effects. The enforcing parties themselves attest the problem is live; external scholarship characterizes the underlying empirical conflict as resolved, which is itself signal that the arrangement now maintains a problem its constituency experiences as real.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__young_earth_literal, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__young_earth_literal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__young_earth_literal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_cosmology__young_earth_literal, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__young_earth_literal, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__young_earth_literal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_cosmology__young_earth_literal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement transfers money upward (tithes, admissions, curriculum sales), transfers epistemic authority from evidence to text, and imposes real costs on identifiable outsider and dissenter seats while the coordinating benefits accrue concentratedly to the institutions. Suppression is higher still (0.78) and is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope. The suppression series traces a distinctive arc: statutory suppression peaked in the 1920s anti-evolution-law era, receded through mid-century as bans lapsed and textbook coverage softened voluntarily, then rebuilt from the 1960s onward as organized 'scientific creationism' campaigns, equal-time legislation, and — after the judicial defeats of the late 1980s and 2000s — migration of enforcement into private-schooling, homeschooling, and congregational-discipline channels. Net applied suppression at interval end exceeds the 1925 level even though the legal channel has contracted. Theater rises steadily (0.20 to 0.47) as the apologetics output increasingly serves insider reassurance — museums, pseudo-research programs, debate performances — alongside the genuine identity-maintenance function; it approaches but stays below the Goodhart threshold because the coordination function still demonstrably works for its participants. All three series are authored on one shared seven-point grid (1925-2025) so the engine samples every metric at every examined time point. Accessibility collapse is moderate (0.6): inside the enforcing perimeter alternative readings are formally condemned and practically costly, but they flourish immediately outside it, so understanding the doctrine does not collapse alternatives globally. Resistance is substantial (0.7): sustained opposition from the scientific community, repeated judicial reversals, reform organizations, and internal dissent.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute very different types from identical structural data. From the ministry and denominational seats the arrangement is guardianship: they experience enforcement as fidelity, revenue as support, and dissent as threat — their identity fusion (the organization has become its function) makes exit unthinkable even where it is formally possible. From the student seat the same structure is a closed information environment they did not choose and cannot leave; from the teacher seat it is chronic professional compromise; from the dissenter seat it is a choice between authenticity and belonging. The scientist seat diverges sharply from the student seat despite both being targets: mobile exit (journals, universities, funding unaffected outside the perimeter) damps the scientist's experienced cost, while the student's trapped position concentrates it. Lay believers occupy the pivotal dual position — net beneficiaries by their own lights, while funding the apparatus and absorbing collateral costs when science intrudes. The engine computes this per-seat divergence from the authored power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: creationist_ministries and denominational_authorities sit near the subsidy end (the arrangement pays them), with their identity_locked exits amplifying commitment rather than exposure. Lay_believers derive near-symmetric directionality — genuine coordination benefit against diffuse funding and collateral costs. Victims derive high directionality, modulated by exit: public_school_students (trapped, powerless) sit nearest the full-target end; doctrinal_dissenters (identity_locked) are amplified toward full-target because their exit carries identity cost; biology_teachers (constrained) sit somewhat below them; scientific_community (mobile, institutional) is a genuine target within the enforcement perimeter whose mobility damps effective extraction below what raw targeting would suggest. Theistic_evolution_advocates are excluded rather than coordinated — outside the transfer path. Courts are analytically neutral. No directionality overrides are authored: the derivation chain from roles, power, and exit options already separates these seats correctly, and the story's mixed power atoms (institutional covers both a beneficiary and a target) would make atom-keyed overrides collide rather than correct.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving plain-sense textual authority against apparent empirical contradiction — remains live for the enforcing constituency, so the mandate has not outlived its function and mandatrophy is not resolved. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: aligned, no capture/zombie flag fires, and the computed piton/theater path corroborates (theater_ratio 0.47, below the drift threshold). The classification work this story performs is keeping the two failure modes apart: labeling the arrangement pure extraction ignores the real identity-coordination function that voluntary participants demonstrably consume and fund; labeling it pure coordination ignores the named victims, the enforcement machinery, and the suppressed alternatives that make participation costly for outsiders and dissenters. The tangled-rope structure — coordination function plus asymmetric extraction plus active enforcement — is the honest reading of the structural data, and the rising theater series marks where degradation would next appear if the coordination function were to hollow out while enforcement persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the young_earth_literal reading of the genesis_creation_cosmology kernel; which structural features belong to the reading versus the kernel, and what would the sibling readings change?',
    'Comparative analysis across the three reading-level stories: diff the beneficiary/victim sets, enforcement flags, and epsilon; features constant across readings are kernel-level, features unique to this file are reading-indexed.',
    'If the victim set and suppression profile are reading-indexed, the sibling stories should compute materially lower effective extraction and different types; if they are kernel-level, the whole family shares one classification and the decomposition was cosmetic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed versus kernel-level structural features of the Genesis-cosmology family.').

omega_variable(
    conviction_vs_revenue_persistence,
    'Is the doctrine''s persistence driven primarily by sincere conviction among holders or by institutional dependence on the revenue and cohesion it generates?',
    'Track institutional behavior where revenue and conviction decouple: donor-base shifts, generational attrition surveys, and whether institutions soften the doctrine when enforcement costs exceed receipts.',
    'If revenue dependence dominates, the arrangement drifts toward captured maintenance with rising theater; if conviction dominates, persistence is preference-driven and enforcement is expressive rather than acquisitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_revenue_persistence, empirical, 'Conviction-driven versus revenue-driven persistence of the doctrine.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression structural (institutional discipline, curriculum control, platform exclusion) or internalized (self-silencing, anticipatory conformity, doubt trained out of members)?',
    'Post-exit trajectory study: whether former members continue to self-censor and defer to textual authority after leaving the enforcing perimeter.',
    'If internalized, effective suppression exceeds the structural measure — leavers carry the arrangement with them — and the identity_locked exit ratings understate how much freedom remains after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized suppression mechanism in the enforcing communities.').

omega_variable(
    authority_grounding_framing_underdetermination,
    'Does the enforcing authority ground its legitimacy in lineage (continuity with the text and the plain-sense tradition) or in extraction (institutional benefit from preventing kernel revision)?',
    'Test which framing predicts behavior: lineage predicts the doctrine held even at financial loss; extraction predicts the doctrine adjusted to protect revenue streams. Compare institutional responses to declining enrollment and shifting donor bases.',
    'Under the extraction framing the authority structure classifies as drift-denial-based, raising estimated effective extraction and shifting the commitment-system pattern; under lineage the same conduct reads as traditional guardianship. The lineage framing is declared here; the extraction framing is the live alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing_underdetermination, conceptual, 'Two coherent framings of the enforcing authority''s legitimacy source, with different classification consequences.').

omega_variable(
    enforcement_dependence_of_extraction,
    'Is the measured cost structure a property of the doctrine itself or of particular enforcement regimes — some holders of the reading impose nothing on anyone?',
    'Compare cost incidence across enforcement contexts: congregations holding the doctrine privately, denominations requiring confessional affirmation, and jurisdictions shaping public pedagogy.',
    'If costs vanish in non-enforcing contexts, the operative constraint is the enforcement apparatus rather than the belief, and epsilon properly attaches to the apparatus; if costs follow the belief across contexts, the doctrine itself carries the load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependence_of_extraction, conceptual, 'Whether the cost structure belongs to the doctrine or to its enforcement regimes.').

omega_variable(
    chronological_anchor_elasticity,
    'How rigid is the ~6000-10000 year anchor, given the reading''s own historical drift from Ussher''s 4004 BC date?',
    'Track the accepted age-band across the enforcing institutions'' publications over time; widening bands show the reading absorbing empirical pressure by stretching the anchor rather than revising the kernel.',
    'High elasticity predicts continued absorption without type change; rigidity predicts rupture events (schisms, defections) when the anchor is directly challenged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chronological_anchor_elasticity, empirical, 'Elasticity of the chronological anchor under empirical pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__young_earth_literal, 1925, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_yec_tr_t1925, genesis_creation_cosmology__young_earth_literal, theater_ratio, 1925, 0.2).
narrative_ontology:measurement_basis(genesis_yec_tr_t1925, observed).
narrative_ontology:measurement(genesis_yec_tr_t1950, genesis_creation_cosmology__young_earth_literal, theater_ratio, 1950, 0.24).
narrative_ontology:measurement_basis(genesis_yec_tr_t1950, observed).
narrative_ontology:measurement(genesis_yec_tr_t1965, genesis_creation_cosmology__young_earth_literal, theater_ratio, 1965, 0.29).
narrative_ontology:measurement_basis(genesis_yec_tr_t1965, observed).
narrative_ontology:measurement(genesis_yec_tr_t1980, genesis_creation_cosmology__young_earth_literal, theater_ratio, 1980, 0.34).
narrative_ontology:measurement_basis(genesis_yec_tr_t1980, observed).
narrative_ontology:measurement(genesis_yec_tr_t1995, genesis_creation_cosmology__young_earth_literal, theater_ratio, 1995, 0.39).
narrative_ontology:measurement_basis(genesis_yec_tr_t1995, observed).
narrative_ontology:measurement(genesis_yec_tr_t2010, genesis_creation_cosmology__young_earth_literal, theater_ratio, 2010, 0.43).
narrative_ontology:measurement_basis(genesis_yec_tr_t2010, observed).
narrative_ontology:measurement(genesis_yec_tr_t2025, genesis_creation_cosmology__young_earth_literal, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(genesis_yec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(genesis_yec_be_t1925, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 1925, 0.5).
narrative_ontology:measurement_basis(genesis_yec_be_t1925, observed).
narrative_ontology:measurement(genesis_yec_be_t1950, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 1950, 0.53).
narrative_ontology:measurement_basis(genesis_yec_be_t1950, observed).
narrative_ontology:measurement(genesis_yec_be_t1965, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement_basis(genesis_yec_be_t1965, observed).
narrative_ontology:measurement(genesis_yec_be_t1980, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement_basis(genesis_yec_be_t1980, observed).
narrative_ontology:measurement(genesis_yec_be_t1995, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 1995, 0.69).
narrative_ontology:measurement_basis(genesis_yec_be_t1995, observed).
narrative_ontology:measurement(genesis_yec_be_t2010, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement_basis(genesis_yec_be_t2010, observed).
narrative_ontology:measurement(genesis_yec_be_t2025, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(genesis_yec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(genesis_yec_su_t1925, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement_basis(genesis_yec_su_t1925, observed).
narrative_ontology:measurement(genesis_yec_su_t1950, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 1950, 0.61).
narrative_ontology:measurement_basis(genesis_yec_su_t1950, observed).
narrative_ontology:measurement(genesis_yec_su_t1965, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement_basis(genesis_yec_su_t1965, observed).
narrative_ontology:measurement(genesis_yec_su_t1980, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 1980, 0.67).
narrative_ontology:measurement_basis(genesis_yec_su_t1980, observed).
narrative_ontology:measurement(genesis_yec_su_t1995, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 1995, 0.71).
narrative_ontology:measurement_basis(genesis_yec_su_t1995, observed).
narrative_ontology:measurement(genesis_yec_su_t2010, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement_basis(genesis_yec_su_t2010, observed).
narrative_ontology:measurement(genesis_yec_su_t2025, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(genesis_yec_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__young_earth_literal, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__theistic_evolution).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__literary_framework).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Genesis says about creation' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. The readings differ in epsilon (this reading highest), in victim sets (only this reading places scientific consensus, students, teachers, and dissenters inside its cost structure), and in enforcement requirements (only this reading requires active suppression of rival readings and pedagogy). All three stories link via affects_constraints. Direction of influence: this reading's enforcement activity creates the legitimacy pressure to which the sibling readings respond — the siblings are articulated as alternatives to literalism, so this story sits upstream of them in the family's causal structure even though their epsilon values are lower.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
