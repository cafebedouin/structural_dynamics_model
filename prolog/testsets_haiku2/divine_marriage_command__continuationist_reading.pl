% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading): Polygamy Doctrinally Valid Under Suspension
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the CONTINUATIONIST READING of the
 *   contested kernel 'divine_marriage_command'. The reading claims that
 *   Joseph Smith's 1843 revelation commanding polygamy remains doctrinally
 *   valid; the 1890 Manifesto suspending the practice is interpreted as
 *   prudential response to federal coercion, not as doctrinal rescission.
 *   Under this reading, polygamy practitioners and splinter communities
 *   remain theologically legitimate; federal law is an external constraint,
 *   not an internal revision to doctrine. The constraint operates through the
 *   tension between this reading's claim to doctrinal continuity and the
 *   mainstream church's enforcement of the substitutionist reading (which
 *   treats the Manifesto as new revelation rescinding polygamy). This story
 *   is ONE OF THREE readings of the same kernel — the others are
 *   coercion_visibility_reading (acknowledges federal coercion as the
 *   legitimacy source) and substitutionist_reading (Manifesto is new
 *   revelation, not suspension). This story does not encompass those
 *   alternatives; each is a separate constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading): Polygamy Doctrinally Valid Under Suspension").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '7abb9556-321c-4a93-8cc0-dbed4da9b795').
narrative_ontology:cs_kernel_codification('7abb9556-321c-4a93-8cc0-dbed4da9b795', fixed_text).
narrative_ontology:cs_authority_grounding('7abb9556-321c-4a93-8cc0-dbed4da9b795', lineage).
narrative_ontology:cs_interpretation_layer_present('7abb9556-321c-4a93-8cc0-dbed4da9b795').
narrative_ontology:cs_reading_relation('7abb9556-321c-4a93-8cc0-dbed4da9b795', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('7abb9556-321c-4a93-8cc0-dbed4da9b795', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('7abb9556-321c-4a93-8cc0-dbed4da9b795', foundational, prophetic_revelation_inalienable).
narrative_ontology:cs_axiom_status(prophetic_revelation_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('7abb9556-321c-4a93-8cc0-dbed4da9b795', prophetic_revelation_inalienable, theological).
narrative_ontology:cs_axiom('7abb9556-321c-4a93-8cc0-dbed4da9b795', foundational, external_coercion_noninvalidating_doctrine).
narrative_ontology:cs_axiom_status(external_coercion_noninvalidating_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7abb9556-321c-4a93-8cc0-dbed4da9b795', external_coercion_noninvalidating_doctrine, deontological).
narrative_ontology:cs_reference_frame('7abb9556-321c-4a93-8cc0-dbed4da9b795', joseph_smith_original_revelation).
narrative_ontology:cs_drift_state('7abb9556-321c-4a93-8cc0-dbed4da9b795', contemporary_federal_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7abb9556-321c-4a93-8cc0-dbed4da9b795', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, doctrinal_continuity_advocates).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamist_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, splinter_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, splinter_community_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, prophetic_revelation_inalienable).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, external_coercion_cannot_rescind_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the reading that the Manifesto (1890) is a prudential suspension imposed by federal prosecution, not a doctrinal rescission. They claim continuity with Joseph Smith's original revelation on polygamy; the authority and legitimacy of their institutional lineage depends on this reading. They administer the constraint through communal reinforcement, patriarchal authority structures, and kinship networks that encode the doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities, agenda_setter).

% Enforces the substitutionist reading (Manifesto rescinded polygamy as new revelation) in institutional doctrine and membership discipline. They have strategic interest in distancing the mainstream church from splinter groups and maintaining federal legal compliance. They control temple access, ordination authority, and formal theological publication.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the direct legal, social, and kinship costs of living under this reading. They face federal prosecution (now rare but still possible), social stigma, exclusion from mainstream church membership and rituals, and internal pressure from splinter community enforcement of plural marriage norms. Exit means either apostasy (severing religious identity) or submission to mainstream substitutionist authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamist_practitioners, payer,
    powerless, biographical, trapped, local).

% Live within communities structured around the continuationist reading: they receive religious belonging and community identity, but pay through conformity to polygamous marriage norms, restricted exit (apostasy costs relational identity), and submission to communal patriarchal authority that the reading legitimates. Women especially bear the cost of the reading's gender hierarchy embedded in plural marriage theology.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, splinter_community_members, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, splinter_community_members, beneficiary).

% Prosecutes polygamy as criminal and bars polygamous marriage recognition. The system is external to the theological contest but produces the coercive pressure that makes the continuationist vs. substitutionist reading distinction meaningful. Federal enforcement creates the condition under which the reading claim 'Manifesto is suspension, not rescission' retains practical significance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_legal_system, observer,
    institutional, generational, analytical, national).

% Scholars, theologians, and community interpreters who defend the continuationist reading as doctrinally coherent and historically accurate. They benefit from the reading's vindication of their interpretive work and claim to ancestral authenticity. They have limited enforcement power but significant interpretive authority within splinter and some mainstream academic spaces.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, doctrinal_continuity_advocates, beneficiary,
    moderate, biographical, constrained, global).

% Do not directly administer the constraint but are affected by it through institutional messaging that distances the mainstream from splinters and through the theological narrative that frames the Manifesto as rescission (substitutionist reading). They experience the constraint as settled doctrine; most are unaware of the ongoing reading contest.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_members, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of Joseph Smith's 1843 revelation on polygamy (D&C 132) with institutional survival: the continuationist reading permits doctrinal continuity with original revelation while accommodating federal legal prohibition through the framing of 'prudential suspension under duress' rather than doctrinal revision.
% TRANSFER_FUNCTION: Moves religious authority and legitimacy from the mainstream institutional church (which enforces substitutionist reading) to splinter communities and doctrinal-continuity advocates (who maintain continuationist reading). Transfers social and kinship conformity costs from mainstream members to polygamist practitioners and splinter community members, especially women, through enforcement of plural marriage norms.
% ABSENT_VOICES: Women in polygamist communities and splinter groups who experience the reading as constraining but lack formal voice in theological interpretation; federal prosecutors and victim-advocacy groups who would argue the reading obscures harm in plural marriage; mainstream church members who have accepted the substitutionist reading and have no standing to contest it within institutional hierarchy.
% DISAPPEARANCE_RATIONALE: Continuationist reading advocates argue that without this reading, the fundamental authority and continuity of their lineage vanishes — the revelation becomes rescinded rather than suspended, and their communities lose doctrinal legitimacy. Mainstream church institutional authority argues that the substitutionist reading is precisely what permits institutional survival and federal compliance. If the continuationist reading disappeared (replaced by substitutionist reading), splinter communities would face a crisis of authority; if substitutionist reading disappeared (replaced by continuationist reading), mainstream church would face federal legal crisis and membership hemorrhage.
% FOUNDING_PROBLEM: Joseph Smith received and recorded a revelation commanding polygamy (D&C 132, 1843). The federal government criminalized polygamy and prosecuted practitioners in the 1880s. The church president issued the Manifesto (1890) suspending the practice. The theological problem: does the Manifesto rescind the prior revelation (substitutionist reading) or merely suspend its practice under duress while leaving doctrine intact (continuationist reading)?
% FOUNDING_PROBLEM_CORROBORATION: Historians and scholars outside fundamentalist communities document that Joseph Smith's 1843 revelation exists in church archives; federal prosecutors' records confirm the coercive legal pressure; the Manifesto's text is ambiguous on whether it rescinds or suspends doctrine, which is why the reading contest persists. Continuationist scholars cite historical testimony from church leadership indicating the Manifesto was tactical, not doctrinal. Substitutionist scholars cite subsequent prophetic statements treating the Manifesto as new revelation. The contest is attested from multiple independent institutional seats, not self-asserted by the benefiting parties alone.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, contested).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval (135 years from 1890 Manifesto to present), driven by the increasing gap between doctrinal claim (polygamy remains valid) and institutional practice (mainstream church enforces prohibition). Theater ratio rises from 0.22 to 0.52, showing that the continuationist reading's enforcement increasingly relies on identity-maintenance narrative rather than on actual polygamous practice (splinter communities are geographically isolated, legally vulnerable, and demographically small; the reading persists through doctrinal assertion and kinship continuity rather than through widespread operation). Suppression requirement plateaus at 0.71 — the constraint's persistence depends on active enforcement at the community level (patriarchal authority, identity-locked exit options, restrictive communication) and on the mainstream church's active delegitimation of the reading (institutional hierarchy gates its narrative against splinter interpretations). The measurements are authored on one shared time grid so every metric is valued at every examined time point, enabling temporal analysis of drift. The extraction accumulation trajectory suggests this reading functions increasingly as a post-hoc legitimation of community choice rather than as a living doctrine governing institutional behavior.
 *
 * PERSPECTIVAL GAP:
 *   This reading should compute as tangled_rope from the splinter agenda-setter seat (genuine coordination of doctrinal interpretation and community identity) + asymmetric extraction (practitioners bear kinship costs). From the polygamist-practitioner seat it should compute as snare (the reading legitimates the community but traps practitioners in conformity). From the mainstream church seat it should compute as piton (the reading is mostly performative — splinters are statistically insignificant, but mainstream enforcement energy is spent on narrative maintenance against the reading rather than on actual doctrinal governance). The engine computes these divergent types from the structural data; this commentary identifies where they should differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Splinter agenda-setters (fundamentalist_splinter_communities) have low d because they benefit from the reading's vindication of their authority and interpret the constraint as genuine doctrinal coordination. Polygamist practitioners have high d because they are structurally trapped — the reading legitimates their lifestyle within community but exposes them to federal prosecution and identity-lock prevents exit. Splinter community members (secondary_role beneficiary) have mixed positioning because they receive religious identity and belonging but pay through conformity to plural marriage norms that the reading justifies. This asymmetry should produce the tangled_rope classification at the beneficiary seats and snare at the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile Joseph Smith's polygamy revelation with federal prohibition) remains live in the reading's own terms, but the practical function has atrophied. The Manifesto solved the immediate crisis (federal prosecution, institutional survival) in 1890. The continuationist reading persists not because it solves that founding problem (the problem is solved by compliance with federal law), but because it provides legitimacy narrative for splinter communities that have chosen to continue plural marriage despite federal prohibition. The theater_ratio rising to 0.52 indicates that over half the constraint's enforcement activity is now performative — maintaining doctrinal claim against mainstream substitutionist interpretation — rather than functional coordination of plural marriage practice (which is geographically isolated and demographically small). This suggests the constraint has moved toward piton classification at the aggregate level, even though it remains tangled_rope at the splinter agenda-setter seat and snare at the practitioner seat. The mandatrophy declaration here is that the original institutional coordination function (reconciling Joseph Smith's revelation with federal law in 1890) has been replaced by legitimacy-narrative maintenance for splinter communities, and the constraint persists primarily through identity-lock and community enforcement, not through the doctrinal coordination it claims to provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rescission_vs_suspension_hermeneutics,
    'Is the Manifesto''s silence on doctrinal rescission evidence that rescission did not occur (continuationist interpretation) or evidence that the text deliberately avoided explicit contradiction of prior revelation (substitutionist counter-interpretation)?',
    'Textual forensics and authorial-intent scholarship: analysis of Joseph F. Smith''s (author of Manifesto) other writings and contemporaneous correspondence; comparison with how previous doctrinal changes were framed in church canon.',
    'If the silence is read as evidence for non-rescission (continuationist premise holds), the reading is structurally stronger. If authorial scholarship establishes that deliberate silence was a political strategy (substitutionist counter), the continuationist reading loses its textual foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rescission_vs_suspension_hermeneutics, empirical, 'Hermeneutic ambiguity in the Manifesto document itself regarding whether rescission occurred.').

omega_variable(
    coercion_as_doctrinal_invalidator,
    'Does federal coercion constitute grounds for doctrinal invalidity? That is, can a reading that asserts ''the Manifesto is not doctrinally binding because it was coerced'' override a reading that treats the Manifesto as binding new revelation?',
    'Doctrinal precedent: comparison with other instances where the church treated institutional pressure as grounds for suspending or revoking prior doctrine (e.g., priesthood restriction lifted in 1978). Jurisprudential analysis of whether ''coercion invalidates doctrine'' is a principle the reading''s own tradition accepts.',
    'If coercion can invalidate doctrine, the continuationist reading gains structural support — the Manifesto is both externally coerced AND doctrinally non-binding. If coercion does not invalidate doctrine (the church''s usual position), continuationist claims about suspension hang on textual silence alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_as_doctrinal_invalidator, conceptual, 'Whether federal coercion can serve as grounds for doctrinal invalidity within the reading''s own framework.').

omega_variable(
    identity_lock_mechanism_origins,
    'Is the high exit cost for splinter community members intrinsic to the religious doctrine itself, or is it maintained through social coercion mechanisms (kinship severance, excommunication, community isolation) that are analytically separable from the doctrine?',
    'Post-exit trajectories: interview and survey data from individuals who left fundamentalist communities; comparison with exit experiences in mainstream church membership (which permits polygamist reading denial without kinship severance).',
    'If exit cost is intrinsic to the doctrine (identity-locked), the constraint''s effective suppression is as high as authored (0.71). If exit cost derives from community enforcement mechanisms, suppression could be reduced by opening communication channels or permitting doctrinal pluralism within mainstream church; the reading would be less extractive under different institutional conditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_origins, empirical, 'Whether the reading''s identity-lock derives from doctrine itself or from community enforcement mechanisms.').

omega_variable(
    women_agency_in_continuationist_framing,
    'Does the continuationist reading''s vindication of plural marriage legitimacy constitute genuine theological grounding for women''s choice to enter polygamous unions, or does it primarily provide theological cover for patriarchal household authority?',
    'Ethnographic and interview research within splinter communities distinguishing women''s articulated reasons for remaining polygamist from institutional incentive structures and economic dependency.',
    'If women articulate genuine theological commitment independent of economic dependency or identity-lock, the reading provides real coordination benefit. If women''s participation is primarily maintained through material dependency and identity-locked exit, the reading is more purely extractive (higher χ at women-target seats).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_agency_in_continuationist_framing, empirical, 'Degree of women''s agency in continuationist reading acceptance within splinter communities.').

omega_variable(
    reading_future_lifecycle,
    'Is the continuationist reading experiencing its end-of-life? Demographic trends show declining splinter community membership, reduced federal prosecution, and generational attrition; does the reading persist only through institutional inertia and identity-locked kinship transmission?',
    'Longitudinal demographic analysis of splinter community populations; generational cohort study of inherited vs. chosen adherence to the reading; measurement of institutional investment in continuationist theological education.',
    'If the reading is entering Piton (atrophied function, theatrical maintenance), it will compute as such within 1–2 generational cycles; current tangled_rope/snare classification may be transient. If the reading stabilizes through renewed theological articulation or political identity (e.g., conservative Christian networks), it may persist indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_future_lifecycle, empirical, 'Lifecycle trajectory of the continuationist reading itself — whether it is stabilizing or entering post-functional decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t27, divine_marriage_command__continuationist_reading, theater_ratio, 27, 0.31).
narrative_ontology:measurement_basis(divi_tr_t27, observed).
narrative_ontology:measurement(divi_tr_t54, divine_marriage_command__continuationist_reading, theater_ratio, 54, 0.41).
narrative_ontology:measurement_basis(divi_tr_t54, observed).
narrative_ontology:measurement(divi_tr_t81, divine_marriage_command__continuationist_reading, theater_ratio, 81, 0.48).
narrative_ontology:measurement_basis(divi_tr_t81, observed).
narrative_ontology:measurement(divi_tr_t108, divine_marriage_command__continuationist_reading, theater_ratio, 108, 0.51).
narrative_ontology:measurement_basis(divi_tr_t108, observed).
narrative_ontology:measurement(divi_tr_t135, divine_marriage_command__continuationist_reading, theater_ratio, 135, 0.52).
narrative_ontology:measurement_basis(divi_tr_t135, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t27, divine_marriage_command__continuationist_reading, base_extractiveness, 27, 0.52).
narrative_ontology:measurement_basis(divi_be_t27, observed).
narrative_ontology:measurement(divi_be_t54, divine_marriage_command__continuationist_reading, base_extractiveness, 54, 0.61).
narrative_ontology:measurement_basis(divi_be_t54, observed).
narrative_ontology:measurement(divi_be_t81, divine_marriage_command__continuationist_reading, base_extractiveness, 81, 0.66).
narrative_ontology:measurement_basis(divi_be_t81, observed).
narrative_ontology:measurement(divi_be_t108, divine_marriage_command__continuationist_reading, base_extractiveness, 108, 0.67).
narrative_ontology:measurement_basis(divi_be_t108, observed).
narrative_ontology:measurement(divi_be_t135, divine_marriage_command__continuationist_reading, base_extractiveness, 135, 0.68).
narrative_ontology:measurement_basis(divi_be_t135, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__continuationist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t27, divine_marriage_command__continuationist_reading, suppression_requirement, 27, 0.62).
narrative_ontology:measurement_basis(divi_su_t27, observed).
narrative_ontology:measurement(divi_su_t54, divine_marriage_command__continuationist_reading, suppression_requirement, 54, 0.67).
narrative_ontology:measurement_basis(divi_su_t54, observed).
narrative_ontology:measurement(divi_su_t81, divine_marriage_command__continuationist_reading, suppression_requirement, 81, 0.7).
narrative_ontology:measurement_basis(divi_su_t81, observed).
narrative_ontology:measurement(divi_su_t108, divine_marriage_command__continuationist_reading, suppression_requirement, 108, 0.71).
narrative_ontology:measurement_basis(divi_su_t108, observed).
narrative_ontology:measurement(divi_su_t135, divine_marriage_command__continuationist_reading, suppression_requirement, 135, 0.71).
narrative_ontology:measurement_basis(divi_su_t135, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.14).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'divine_marriage_command' kernel family. The kernel itself is Joseph Smith's 1843 polygamy revelation and its relationship to the 1890 Manifesto. Three readings decompose the kernel into three structurally distinct constraints: (1) continuationist_reading (this story) — polygamy remains doctrinally valid, Manifesto is suspension under duress; (2) substitutionist_reading — Manifesto represents new revelation rescinding prior command, monogamy now doctrinally required; (3) coercion_visibility_reading — Manifesto's legitimacy derives from institutional survival necessity, federal coercion is acknowledged as the constraint source. Each reading produces different beneficiary/victim structures, enforcement mechanisms, and classification outcomes. All three readings operate simultaneously across different institutional seats (splinter communities, mainstream hierarchy, academic theologians). The constraint family links are recorded in network.affects_constraints; the ε-invariance principle requires separate constraints for each reading because the referent (the standing doctrinal arrangement under contest) is the same, but the readings' interpretation of that arrangement differs structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
