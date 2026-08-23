% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: The 1890 Manifesto as Federally Coerced Suspension of Practice (Exogenous Override Reading)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   The 1890 Manifesto announced the suspension of new plural marriages in
 *   the LDS Church after twenty-eight years of escalating federal pressure
 *   culminating in the Edmunds-Tucker Act, corporate dissolution, and
 *   property receivership. This file instantiates ONE reading of the
 *   contested kernel marriage_commitment_legitimacy: the
 *   exogenous_override_reading, under which the Manifesto was capitulation to
 *   federal force — theological doctrine unchanged, only practice suspended
 *   under duress. On this reading the standing arrangement (the enforced
 *   suspension) takes a binding covenant's practice from believers while its
 *   gains accrue to the federal state and to the surviving institution, and a
 *   legitimacy gap opens as members compare the material record with the
 *   official spiritual framing. Per the epsilon-invariance principle this
 *   reading is authored alone, as a clean constraint with one stable epsilon;
 *   the endogenous and hybrid readings are other files over the same
 *   referent, linked through the network section. The claimed type
 *   (tangled_rope) and the metrics are independent authored facts: the claim
 *   states the structure I believe true (a real settlement function joined to
 *   asymmetric extraction under active enforcement); the metrics describe the
 *   arrangement's operation as this reading sees it. The epsilon referent is
 *   the standing Manifesto regime, assessed by this reading's own lights —
 *   never the arrangement any sibling reading would endorse.
 *
 * KEY AGENTS:
 *   - us_federal_government: Primary beneficiary and agenda-setter (institutional/arbitrage) — extracted compliance through statute, prosecution, and receivership; conceded nothing doctrinal
 *   - lds_church_presidency: Administering agent under duress (powerful/constrained) — issued and then enforced the suspension while absorbing its legitimacy cost
 *   - lds_church_as_institution: Secondary beneficiary and cost-bearer (institutional/constrained) — traded covenant practice for survival, property recovery, and statehood
 *   - believing_plural_marriage_members: Primary target (moderate/identity_locked) — hold the covenant as eternally binding; bear the suspension where exit is least thinkable
 *   - existing_plural_families: Concentrated victim seat (powerless/trapped) — frozen households with no legal path and no acceptable dissolution
 *   - post_manifesto_practitioners: Deferred-cost target (moderate/constrained) — married 1890-1904 under tacit permission, disciplined after the Second Manifesto
 *   - mormon_border_colonies: Partial-exit seat, incidental beneficiary turned evacuee (organized/constrained)
 *   - academic_and_journalistic_observers: Analytical observer (analytical/analytical) — document the coercion record independent of official framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.71).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "The 1890 Manifesto as Federally Coerced Suspension of Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'f3295a54-f77f-47dd-b0f3-a4775129760e').
narrative_ontology:cs_kernel_codification('f3295a54-f77f-47dd-b0f3-a4775129760e', fixed_text).
narrative_ontology:cs_authority_grounding('f3295a54-f77f-47dd-b0f3-a4775129760e', lineage).
narrative_ontology:cs_interpretation_layer_present('f3295a54-f77f-47dd-b0f3-a4775129760e').
narrative_ontology:cs_reading_relation('f3295a54-f77f-47dd-b0f3-a4775129760e', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('f3295a54-f77f-47dd-b0f3-a4775129760e', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('f3295a54-f77f-47dd-b0f3-a4775129760e', foundational, coerced_suspension_is_not_doctrinal_authority).
narrative_ontology:cs_axiom_status(coerced_suspension_is_not_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('f3295a54-f77f-47dd-b0f3-a4775129760e', coerced_suspension_is_not_doctrinal_authority, deontological).
narrative_ontology:cs_axiom('f3295a54-f77f-47dd-b0f3-a4775129760e', foundational, plural_marriage_remains_eternal_principle).
narrative_ontology:cs_axiom_status(plural_marriage_remains_eternal_principle, holdable).
narrative_ontology:cs_axiom_grounding('f3295a54-f77f-47dd-b0f3-a4775129760e', plural_marriage_remains_eternal_principle, theological).
narrative_ontology:cs_reference_frame('f3295a54-f77f-47dd-b0f3-a4775129760e', revealed_doctrine_openly_practiced).
narrative_ontology:cs_drift_state('f3295a54-f77f-47dd-b0f3-a4775129760e', post_second_manifesto_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f3295a54-f77f-47dd-b0f3-a4775129760e', '2026-06-12T09:00:00Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, us_federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_as_institution).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, believing_plural_marriage_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, existing_plural_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, mormon_border_colonies).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_as_institution).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_presidency).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, mormon_border_colonies).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_constitutional_supremacy_over_religious_practice).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, reynolds_v_united_states_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducted a multi-decade campaign to bring plural marriage within reach of federal law: the Edmunds and Edmunds-Tucker Acts, prosecutions, disfranchisement, and corporate dissolution of the church with its property placed under receivership. When the church president announced suspension of new plural marriages, it accepted the announcement as sufficient, opened the path to Utah statehood and property recovery, and stood down active enforcement. It conceded nothing doctrinal and collected the jurisdictional settlement at minimal further cost.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, us_federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Faced dissolution: charter revoked, temples and real estate in receivership, leadership imprisoned or in hiding. After the announcement it recovered most property through congressional action, gained statehood for its home territory, and re-entered ordinary political and economic life. It survived and grew as a corporation, at the price of administering a rule it did not originate and could not publicly ground in its own theological categories.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_as_institution, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_as_institution, payer).

% Issued the announcement under imminent threat of institutional destruction, then administered compliance: instructing local leaders, answering member inquiries, and after 1904 drawing hard lines through temple-interview standards and disciplinary councils during the Smoot hearings. Successive presidents bore personally the distance between what the church had taught for two generations about eternal covenants and what it now enforced; several had practiced or performed plural marriages themselves. Their alternatives were capitulation or watching the institution be dismantled.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_presidency, agenda_setter,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_presidency, payer).

% Held plural marriage as a revealed, eternally binding covenant preached as a condition of exaltation. After the announcement they were asked to treat the same covenant as suspended: no new sealings of this kind, courtship paths closed, earlier teachings reinterpreted only in private. Leaving the community meant losing family, assurance of salvation, and every social tie; remaining meant living inside a rule their theology described as temporary at best. Their sense of self was constituted by the covenant, so the rule reached them where exit was least thinkable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, believing_plural_marriage_members, payer,
    moderate, generational, identity_locked, regional).

% Households already constituted under the earlier commandment. The announcement froze them: no legal recognition, no lawful expansion, husbands exposed to renewed prosecution if relationships continued openly, wives and children carrying lasting stigma and uncertainty. Dissolving the families was spiritually and practically unthinkable; concealing them was precarious; relocating to the colonies was costly and temporary once Mexico destabilized in 1912.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, existing_plural_families, payer,
    powerless, generational, trapped, regional).

% Between 1890 and 1904 a number of members entered new plural marriages, some with the knowledge or tacit approval of senior leaders, understanding the principle to persist unofficially. When Senate scrutiny made continuation untenable, the Second Manifesto of 1904 drew a hard line; some of these marriages became disciplinary cases, and the participants bore exposure, discipline, or quiet marginalization for acting on permissions the institution had lately seemed to extend.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_practitioners, payer,
    moderate, biographical, constrained, regional).

% Settlements in northern Mexico and southern Alberta founded partly beyond the reach of United States marriage law. They received relocated families and hosted some continued practice, serving as the arrangement's safety valve. The price was frontier hardship, households divided across borders, and finally the loss of the refuge itself when the Mexican revolution reached the colonies in 1912 and forced evacuation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, mormon_border_colonies, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, mormon_border_colonies, payer).

% Contemporary secular newspapers reported the announcement openly as surrender to federal power; later historians reconstructed the sequence from Senate hearing transcripts, diaries, and archival records of post-announcement marriages. Neither body depends on the church's own framing, and together they document the distance between the public spiritual language and the documented coercion behind it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, academic_and_journalistic_observers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, us_federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled the jurisdictional conflict between federal law and ecclesiastical practice: one marriage standard applied across United States territory, ending prosecutions, property receivership, and disfranchisement, and clearing Utah's path to statehood while giving members legal peace.
% TRANSFER_FUNCTION: Moved compliance from the Latter-day Saint community to the federal state: members surrendered the practice of a covenant they held eternally binding; the government received jurisdictional supremacy, reduced enforcement costs, and territorial integration without further expenditure.
% ABSENT_VOICES: Existing plural families and believing members had no seat in the settlement — it was struck between federal authorities and the church presidency. Rank-and-file covenant holders learned of the surrender as a published fait accompli, and the women and children of plural households bore its domestic consequences entirely without voice in either capital.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the church-state settlement unwinds: Utah's statehood timeline, the recovery of church property, the seating of Apostle Reed Smoot, and the legal security ordinary members had gained all depended on it; federal prosecutions resume or the supremacy claim collapses, and either the institution or its covenant practice is destroyed.
% FOUNDING_PROBLEM: A jurisdictional war: the federal state could not tolerate a large organized community practicing a form of marriage its law forbade, and the church could not abandon what it held as revealed covenant without dissolving its own claim to prophetic authority.
% FOUNDING_PROBLEM_CORROBORATION: The original conflict is corroborated from outside the beneficiary set by congressional hearing records, contemporaneous secular press coverage, and non-member academic historians of American religious history. Its status is disputed along the same line: the mainstream church's own centennial retrospectives attest resolution (but speak from inside the arrangement), while fundamentalist descendant communities — themselves outside the beneficiary set — attest the covenant obligation as persisting and the conflict as displaced rather than solved. No single neutral source attests the founding problem as dead.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.71: the arrangement removes a covenant's practice from believers while the settlement goods (statehood, recovered property, legal peace) land with the federal state and the institution. Suppression 0.78 is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and the arrangement's continental scope, which the engine applies. Theater 0.46: public spiritual framing coexisted with documented post-announcement marriages through 1904, peaking during the Smoot hearings when emphatic public renunciation overlapped recently sanctioned private practice. Accessibility collapse 0.62: emigration and quiet continuance narrowed but never fully vanished; resistance 0.55 reflects sustained quiet defiance, apostolic ambivalence, and the fundamentalist secessions that followed. All three tracked metrics run on ONE shared seven-point grid (1890-1912). The trajectory is spike-and-settle rather than cyclical: extraction dips through the statehood honeymoon, spikes at the Second Manifesto when deferred costs land on post-announcement practitioners, and settles elevated. The suppression_requirement series traces enforcement TRANSFER — federal machinery standing down while ecclesiastical enforcement hardened — so the standing enforcement burden stays high even where instantaneous applied force falls. The coercion_grid (authored because level-resolved coercion movement IS this story's subject: structural pressure converting into individual-level gates) reads grid suppression as applied intensity at each level and time, which is why its 1912 structural value sits below the standing-requirement scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the federal seat the episode is a cheap constitutional victory; from the presidency seat, reluctant administration necessary to prevent destruction; from the believer seats, a covenant suspended by a force they must nevertheless obey; from the plural-family seats, a frozen household with no acceptable move. The engine derives these per-seat classifications from power, exit, and directional position — not from any seat's self-description, and notably not from the presidency's public framing of the announcement. Coalition note: the payer seats' theoretical coalition power was real but unusable — the federal state had already demonstrated willingness to imprison and seize property, and the hierarchy controlled the community's coordinating institutions, so class-level refusal never organized at the scale the stakes implied.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives derivation. The federal government (declared beneficiary, arbitrage exit) derives near the full-beneficiary end. Believing members and plural families (declared victims, identity_locked and trapped exits) derive near the full-target end. Two overrides where derivation is silent or misleading: (1) the presidency carries power atom 'powerful' with no beneficiary/victim declaration, so structural derivation produces nothing; it administers a rule it resisted and absorbs the legitimacy cost, placing it slightly target-side of symmetric — override d=0.62. (2) The border colonies are declared beneficiaries, which the derivation would read as substantially subsidized; but their benefit was incidental, partial, and reversed by the 1912 evacuation — override d=0.45 places them near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination would launder a coerced covenant suspension into voluntary reform; reading it as pure extraction would erase the genuine settlement goods that even victim seats received. The tangled_rope claim holds both faces together. On mandatrophy: the founding jurisdictional war ended, but the enforcement function did not atrophy — it migrated inward (temple interviews still ask about this history) and persists without a sunset clause; a transitional measure that never declared its end. The contested founding_problem_status records the live dispute over whether the underlying covenant-law conflict was solved or displaced into fundamentalist dissent, which guards the mismatch consumer against reading a solved-genealogy story off a persistence pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_epsilon_delta,
    'How do epsilon and the computed classification shift if a sibling reading of the marriage_commitment_legitimacy kernel is adopted over the same standing referent?',
    'Compile the sibling stories and compare engine classifications over identical structural data modulo reading-indexed values; the referent is fixed, so divergence isolates the reading index itself.',
    'Under the endogenous reading the same arrangement computes near the coordination floor (voluntary obedience to revelation, negligible excess extraction); the hybrid reading lands intermediate; this reading''s high-extraction profile is therefore a property of the reading, not of the events alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexical_epsilon_delta, conceptual, 'Same referent, three reading-indexed epsilon values — the classification is seat-of-the-reading dependent.').

omega_variable(
    compliance_mechanism_partition,
    'Was member compliance with the suspension produced by structural coercion (statutory penalties, receivership, prosecution risk) or by internalized prophetic deference that persists independently of penalty?',
    'Trajectory analysis of periods and populations where penalties lapsed (the post-statehood honeymoon, colonists beyond US jurisdiction): if compliance-with-private-exception persisted where enforcement was dormant, the internalized share is substantial.',
    'A large internalized share raises effective suppression above the structural measure and predicts persistence of the legitimacy gap across generations; a purely structural reading predicts rapid practice resumption once penalties lifted — which the record contradicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_partition, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    post_manifesto_continuation_extent,
    'How many plural marriages were performed or sanctioned between 1890 and 1904, and with what degree of presidential and apostolic knowledge?',
    'Archival enumeration: Smoot hearing testimony, contemporaneous records, and subsequent scholarship on the post-Manifesto marriage population.',
    'A large sanctioned extent raises the theater_ratio across 1890-1904 (public compliance narrative over continuing practice) and sharpens the 1904 spike as deferred costs land; a small extent makes the official framing substantially less theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_manifesto_continuation_extent, empirical, 'Size and sanction level of the post-announcement marriage population.').

omega_variable(
    indefinite_suspension_threshold,
    'At what duration does an indefinitely suspended practice cease to count as ''doctrine unchanged, practice suspended'' and become de facto doctrinal revision?',
    'Conceptual analysis within the tradition''s own categories: official statements, canon additions, and whether the practice is ever restorable in principle.',
    'Crossing the threshold converts this reading''s foundational axiom into self-contradiction — the reading loses holdability, and the arrangement''s residual operation becomes predominantly theatrical maintenance rather than enforced suspension of anything live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indefinite_suspension_threshold, conceptual, 'Durability threshold at which suspension-only framing fails.').

omega_variable(
    legitimacy_gap_recognition,
    'What share of members experienced the gap between material conditions and spiritual framing as a recognized legitimacy injury, versus absorbing the official framing?',
    'Diaries, periodicals, defection timing, and the rate at which members migrated to fundamentalist movements once those became available.',
    'Wide recognition raises realized victim-seat extraction and resistance; narrow recognition means much of the measured suppression was precautionary against a dissent that had not yet organized, lowering the constraint''s effective suppressive burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_gap_recognition, empirical, 'Breadth of recognized gap between taught covenant and enforced practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_exog_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(mcl_exog_tr_t0, observed).
narrative_ontology:measurement(mcl_exog_tr_t3, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(mcl_exog_tr_t3, observed).
narrative_ontology:measurement(mcl_exog_tr_t6, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement_basis(mcl_exog_tr_t6, observed).
narrative_ontology:measurement(mcl_exog_tr_t11, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 11, 0.44).
narrative_ontology:measurement_basis(mcl_exog_tr_t11, observed).
narrative_ontology:measurement(mcl_exog_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.55).
narrative_ontology:measurement_basis(mcl_exog_tr_t14, observed).
narrative_ontology:measurement(mcl_exog_tr_t17, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 17, 0.5).
narrative_ontology:measurement_basis(mcl_exog_tr_t17, observed).
narrative_ontology:measurement(mcl_exog_tr_t22, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 22, 0.46).
narrative_ontology:measurement_basis(mcl_exog_tr_t22, observed).

% Extraction over time
narrative_ontology:measurement(mcl_exog_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(mcl_exog_be_t0, observed).
narrative_ontology:measurement(mcl_exog_be_t3, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 3, 0.66).
narrative_ontology:measurement_basis(mcl_exog_be_t3, observed).
narrative_ontology:measurement(mcl_exog_be_t6, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(mcl_exog_be_t6, observed).
narrative_ontology:measurement(mcl_exog_be_t11, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 11, 0.64).
narrative_ontology:measurement_basis(mcl_exog_be_t11, observed).
narrative_ontology:measurement(mcl_exog_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.75).
narrative_ontology:measurement_basis(mcl_exog_be_t14, observed).
narrative_ontology:measurement(mcl_exog_be_t17, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 17, 0.73).
narrative_ontology:measurement_basis(mcl_exog_be_t17, observed).
narrative_ontology:measurement(mcl_exog_be_t22, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 22, 0.71).
narrative_ontology:measurement_basis(mcl_exog_be_t22, observed).

% Suppression requirement over time
narrative_ontology:measurement(mcl_exog_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(mcl_exog_su_t0, observed).
narrative_ontology:measurement(mcl_exog_su_t3, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 3, 0.74).
narrative_ontology:measurement_basis(mcl_exog_su_t3, observed).
narrative_ontology:measurement(mcl_exog_su_t6, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(mcl_exog_su_t6, observed).
narrative_ontology:measurement(mcl_exog_su_t11, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 11, 0.66).
narrative_ontology:measurement_basis(mcl_exog_su_t11, observed).
narrative_ontology:measurement(mcl_exog_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.8).
narrative_ontology:measurement_basis(mcl_exog_su_t14, observed).
narrative_ontology:measurement(mcl_exog_su_t17, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 17, 0.77).
narrative_ontology:measurement_basis(mcl_exog_su_t17, observed).
narrative_ontology:measurement(mcl_exog_su_t22, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 22, 0.78).
narrative_ontology:measurement_basis(mcl_exog_su_t22, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=22
narrative_ontology:measurement(mcl_exog_grid_01, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(mcl_exog_grid_02, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(class), 22, 0.65).
narrative_ontology:measurement(mcl_exog_grid_03, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(mcl_exog_grid_04, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(individual), 22, 0.75).
narrative_ontology:measurement(mcl_exog_grid_05, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(mcl_exog_grid_06, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(organizational), 22, 0.7).
narrative_ontology:measurement(mcl_exog_grid_07, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 0, 0.8).
narrative_ontology:measurement(mcl_exog_grid_08, marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse(structural), 22, 0.85).
narrative_ontology:measurement(mcl_exog_grid_09, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 0, 0.35).
narrative_ontology:measurement(mcl_exog_grid_10, marriage_commitment_legitimacy__exogenous_override_reading, resistance(class), 22, 0.15).
narrative_ontology:measurement(mcl_exog_grid_11, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 0, 0.4).
narrative_ontology:measurement(mcl_exog_grid_12, marriage_commitment_legitimacy__exogenous_override_reading, resistance(individual), 22, 0.2).
narrative_ontology:measurement(mcl_exog_grid_13, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 0, 0.3).
narrative_ontology:measurement(mcl_exog_grid_14, marriage_commitment_legitimacy__exogenous_override_reading, resistance(organizational), 22, 0.1).
narrative_ontology:measurement(mcl_exog_grid_15, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 0, 0.15).
narrative_ontology:measurement(mcl_exog_grid_16, marriage_commitment_legitimacy__exogenous_override_reading, resistance(structural), 22, 0.05).
narrative_ontology:measurement(mcl_exog_grid_17, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 0, 0.8).
narrative_ontology:measurement(mcl_exog_grid_18, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(class), 22, 0.6).
narrative_ontology:measurement(mcl_exog_grid_19, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement(mcl_exog_grid_20, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(individual), 22, 0.65).
narrative_ontology:measurement(mcl_exog_grid_21, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 0, 0.85).
narrative_ontology:measurement(mcl_exog_grid_22, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(organizational), 22, 0.6).
narrative_ontology:measurement(mcl_exog_grid_23, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 0, 0.9).
narrative_ontology:measurement(mcl_exog_grid_24, marriage_commitment_legitimacy__exogenous_override_reading, stakes_inflation(structural), 22, 0.55).
narrative_ontology:measurement(mcl_exog_grid_25, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 0, 0.75).
narrative_ontology:measurement(mcl_exog_grid_26, marriage_commitment_legitimacy__exogenous_override_reading, suppression(class), 22, 0.45).
narrative_ontology:measurement(mcl_exog_grid_27, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 0, 0.6).
narrative_ontology:measurement(mcl_exog_grid_28, marriage_commitment_legitimacy__exogenous_override_reading, suppression(individual), 22, 0.6).
narrative_ontology:measurement(mcl_exog_grid_29, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 0, 0.85).
narrative_ontology:measurement(mcl_exog_grid_30, marriage_commitment_legitimacy__exogenous_override_reading, suppression(organizational), 22, 0.55).
narrative_ontology:measurement(mcl_exog_grid_31, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 0, 0.9).
narrative_ontology:measurement(mcl_exog_grid_32, marriage_commitment_legitimacy__exogenous_override_reading, suppression(structural), 22, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims about the SAME standing arrangement, decomposed per the epsilon-invariance principle. This file (exogenous_override_reading) authors high epsilon: coercion, not revelation, produced the suspension, and the gains accrue externally. The endogenous reading authors low epsilon over the identical referent (divine command voluntarily obeyed); the hybrid reading authors intermediate epsilon (real agency within real duress). Upstream/downstream: the documentary coercion record upstream of this reading is cited against the endogenous claim and reshapes the hybrid framing. Each family member links the others through affects_constraints; classifications are comparable only as reading-indexed values over the shared referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, powerful, 0.62).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
