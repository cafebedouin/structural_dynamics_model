% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Polygamy as Doctrinally Continuous (Continuationist Reading)
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   The continuationist reading of the divine-marriage-command kernel holds
 *   that polygamy remains doctrinally valid and that the 1890 Manifesto is a
 *   prudential suspension of practice under federal coercion, not a doctrinal
 *   rescission. This reading is instantiated by fundamentalist splinter
 *   communities that claim continuity with the original revelation and frame
 *   the mainstream institutional church as having compromised under external
 *   pressure. From the continuationist seat, the constraint operates as a
 *   tangled rope: it coordinates the theological community around a specific
 *   hermeneutical principle (revelation is immutable) while extracting from
 *   those who operate under its authority (women in polygamous unions, who
 *   are bound by the doctrine's operative reach; the mainstream institution,
 *   which must defend against the authority challenge). The claim is
 *   tangled_rope; the metrics describe substantial extraction (0.62)
 *   supported by high suppression (0.71) and rising theater (0.58 by 2026),
 *   indicating that performance of doctrinal coherence increasingly dominates
 *   actual functional coordination as the constraint persists.
 *
 * KEY AGENTS:
 *   - Fundamentalist splinter communities: agenda-setters who interpret and enforce the continuationist doctrine; organized power, identity-locked exit, regional scope.
 *   - Mainstream institutional church: constrained payer bearing the cost of schismatic contestation and theological authority challenge; institutional power, generational horizon.
 *   - Women in polygamous unions: powerless targets trapped by the doctrine's operative reach; biographical horizon, local scope.
 *   - Federal government and civil law: observer seat maintaining the external constraint (criminal law against plural marriage) that splinters characterize as coercion necessitating suspension rather than rescission.
 *   - The doctrine itself (vested in the scriptural claim): beneficiary through vindication of the principle that revelation is immutable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.62).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Polygamy as Doctrinally Continuous (Continuationist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '42ae5b7e-f22e-4d71-86d5-31442393c711').
narrative_ontology:cs_kernel_codification('42ae5b7e-f22e-4d71-86d5-31442393c711', fixed_text).
narrative_ontology:cs_authority_grounding('42ae5b7e-f22e-4d71-86d5-31442393c711', lineage).
narrative_ontology:cs_interpretation_layer_present('42ae5b7e-f22e-4d71-86d5-31442393c711').
narrative_ontology:cs_reading_relation('42ae5b7e-f22e-4d71-86d5-31442393c711', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('42ae5b7e-f22e-4d71-86d5-31442393c711', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('42ae5b7e-f22e-4d71-86d5-31442393c711', foundational, revelation_doctrinally_immutable).
narrative_ontology:cs_axiom_status(revelation_doctrinally_immutable, holdable).
narrative_ontology:cs_axiom_grounding('42ae5b7e-f22e-4d71-86d5-31442393c711', revelation_doctrinally_immutable, deontological).
narrative_ontology:cs_axiom('42ae5b7e-f22e-4d71-86d5-31442393c711', secondary, external_coercion_cannot_rescind_doctrine).
narrative_ontology:cs_axiom_status(external_coercion_cannot_rescind_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('42ae5b7e-f22e-4d71-86d5-31442393c711', external_coercion_cannot_rescind_doctrine, deontological).
narrative_ontology:cs_reference_frame('42ae5b7e-f22e-4d71-86d5-31442393c711', restored_polygamist_doctrine_from_revelation).
narrative_ontology:cs_drift_state('42ae5b7e-f22e-4d71-86d5-31442393c711', contemporary_era_2026, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('42ae5b7e-f22e-4d71-86d5-31442393c711', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, women_in_polygamous_unions).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, institutional_churches_claiming_mainstream_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, mainstream_institutional_church).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, mainstream_institutional_church).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, divine_revelation_immutable).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, doctrinal_authority_vested_in_scriptural_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the original revelation as intact and doctrinally valid; treat the 1890 Manifesto as a prudential, coerced suspension, not a doctrinal rescission. They organize communal structures around plural marriage as a sacred principle, claim theological continuity with foundational revelation, and maintain institutional authority over marriage practice independent of civil law. Their exit from this framing would require severing the identity claim that they preserve an essential doctrine the mainstream institution abandoned.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities, agenda_setter,
    organized, generational, identity_locked, regional).

% Bears the cost of disaffiliation schisms and theological contestation over authority when splinters claim the institution doctrinally deviated. Receives institutional stability and legal recognition from civil law aligned with the 1890 Manifesto reading (monogamy-as-doctrine). Exit from monogamy doctrine would reopen the legal jeopardy that the Manifesto addressed; exit from claiming doctrinal authority over the splinters would cede the authority-grounding itself.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_institutional_church, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, mainstream_institutional_church, beneficiary).

% Subject to the continuationist doctrine's operative arrangement; plural marriage is framed as revelation rather than preference, constraining exit options by embedding it in sacred obligation. They bear the material costs of shared marriage (economic precarity, domestic labor concentration, medical/legal vulnerability in jurisdictions where plural marriage is criminalized) and the identity lock that frames exit as apostasy.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, women_in_polygamous_unions, payer,
    powerless, biographical, trapped, local).

% Enforces monogamy law and criminal penalties for plural marriage. In the continuationist reading, federal law is external constraint (coercion), not internal doctrinal authority. The government observes the theological dispute but enforces the civil arrangement that the institutional church publicly adopted via the Manifesto.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_government_jurisdiction, observer,
    institutional, generational, analytical, national).

% The doctrine that revelation remains immutable and that doctrinal authority derives from scriptural claim (not from institutional choice or civil compromise). The continuationist reading vindicates this proposition against the institutional claim that doctrine can be revised by formal pronouncement in response to external pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, doctrinal_authority_claim, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(divine_marriage_command__continuationist_reading, doctrinal_authority_claim).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes the theological community's understanding of divine will regarding marriage: resolves ambiguity about whether the 1890 Manifesto represents internal doctrinal development or external capitulation. The continuationist reading coordinates around the principle that revelation is immutable and that apparent contradiction (plural marriage + Manifesto) must be reconciled through distinguishing timeless doctrine from temporary suspension.
% TRANSFER_FUNCTION: Moves religious authority and legitimacy from the institutional mainstream church (which claims the Manifesto as doctrinal truth) to the splinter communities (which claim continuity with original revelation and frame the mainstream as fallen/compromised). Authority flows to whichever party credibly claims fidelity to immutable divine command.
% ABSENT_VOICES: Women and girls in splinter communities who experience plural marriage as coercive rather than revelatory are structurally excluded from authority over their own interpretation. Secular scholars and religious historians who would dispute that revelation-immutability is itself a settled doctrine are not parties to the theological system. Former members who have left splinter communities and would attest to suppression mechanisms are excluded from theological discourse.
% DISAPPEARANCE_RATIONALE: From the continuationist seat, if this doctrine vanished, splinter communities would lose their theological warrant for claiming continuity with original revelation and would face pressure to either rejoin the mainstream or reorganize around a different authority claim. Institutional authority structures would be simplified. From the mainstream seat, disappearance would eliminate a persistent schism and authority challenge. From the women's seat, disappearance might reduce identity-lock suppression if an alternative framework (doctrine is mutable; plural marriage is culturally outdated, not divinely mandated) became available. The constraint's disappearance would genuinely rearrange theological community coherence, though the rearrangement would be contested — splinters would claim the disappearance itself was a loss of truth.
% FOUNDING_PROBLEM: Early religious revelation includes a divine command authorizing plural marriage (interpreted by communities from scriptural texts as immutable and eternally valid). Later, federal law criminalizes plural marriage and threatens institutional survival. The founding problem is: can the doctrine remain valid while practice is suspended under duress, or does external coercion necessitate doctrinal rescission?
% FOUNDING_PROBLEM_CORROBORATION: Fundamentalist splinter communities attest the problem is live and requires the continuationist solution. Scholarly historians of Mormonism and gender studies scholars attest the theological question remains disputed and unresolved; the mainstream institution's official substitutionist position has not suppressed the continuationist reading's recurrence. No external authority (secular, interfaith, or even the broader Christian tradition) has authoritatively settled the question. The problem persists as a live theological dispute with no consensus resolution.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, contested).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) and rising from 1890 baseline (0.45) because the constraint operates as sustained contestation of authority: splinters extract legitimacy and institutional coherence from the mainstream by claiming that the doctrine remains valid and the institution fell. Suppression is high (0.71) because the constraint's persistence depends on women in unions accepting the doctrine as revelation rather than choice, and on the mainstream institution maintaining the public Manifesto position while the theological dispute continues. Theater is rising (from 0.32 in 1890 to 0.58 by 2026) because the actual functional coordination (theological resolution of the 1890 question) has not been achieved; what persists is increasingly performative maintenance of the boundary between institutional and splinter authority claims. The measurements span one shared time grid (1890–2026 at 26-year intervals) to capture the post-Manifesto trajectory: extractiveness and suppression both rise, indicating the constraint becomes more extractive and harder to escape as time passes and the dispute persists unresolved. Theater rises fastest, suggesting that doctrinal authority is increasingly performed rather than functionally adjudicated. The constraint is CLAIMED as tangled_rope because it coordinates around a principle (doctrine-immutability) while extracting from targets; the metrics support this as structurally accurate rather than aspirational.
 *
 * PERSPECTIVAL GAP:
 *   From the fundamentalist splinter seat, the continuationist doctrine is a genuine preservation of revealed truth against institutional compromise — extraction is zero or negative (they benefit from the authenticity claim). From the mainstream institutional seat, the continuationist doctrine is a persistent authority challenge that costs institutional stability and requires ongoing defense — extraction is high. From the women's seat, the constraint is experienced as obligatory sacred doctrine that cannot be questioned without apostasy — suppression is very high and exit is identity-foreclosed. From the civil government seat (observer), the constraint is theological contestation orthogonal to law enforcement. The engine computes these as per-seat classifications from the structural data: the beneficiary seat (splinters collecting authority legitimacy) gets low d and low/negative χ; the payer seats (mainstream institution, women) get high d and high χ. The authored claim (tangled_rope) reflects the structure: genuine coordination function (around hermeneutical principle) + asymmetric extraction (from some seats to others).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for fundamentalist splinters: d ≈ 0.15–0.25 (beneficiary seat; they collect authority legitimacy by claiming continuity with revelation). Power is organized (institutional coherence) and exit is identity_locked (to leave is to repudiate the doctrine they claim to preserve), which moderates d slightly downward from the pure-beneficiary end. Directionality for mainstream institution: d ≈ 0.65–0.75 (payer seat; bears the cost of schism and authority contestation). Power is institutional (can change institutional doctrine) but exit is constrained (changing doctrine would reopen the original legal jeopardy and validate the splinter authority claim), which moderates d slightly downward from the full-target end. Directionality for women: d ≈ 0.85–0.95 (full target). Power is powerless, exit is trapped (legal vulnerability, economic dependence, social isolation, identity fusion), and spatial scope is local (limited arbitrage options). They bear the constraint's operative force fully. Directionality for doctrine/revelation-immutability principle: d ≈ 0.0 (vindicated beneficiary; gains legitimacy from the constraint's operation but is not an agent that collects or distributes rents). No overrides are necessary; the derivation chain produces accurate d values from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (how to reconcile plural marriage doctrine with federal coercion) was live at 1890 and remains live in 2026. The institutional church officially adopted the substitutionist reading (doctrine changed; plural marriage is no longer divinely authorized), but the continuationist reading persists in splinter communities that claim theological fidelity. Mandatrophy would be resolved if: (1) the mainstream institution formally repudiated the original texts authorizing plural marriage (full doctrinal rescission, not merely suspension); (2) splinter communities abandoned the continuationist claim and rejoined the mainstream; or (3) the founding problem became universally acknowledged as dead by all parties. None of these have occurred. The constraint avoids mandatrophy resolution by maintaining the ambiguity: the Manifesto's language permits both substitutionist and continuationist readings, the institution has not formally condemned the original texts, and splinters continue to claim doctrinal continuity. Theater rising from 0.32 to 0.58 indicates that the constraint increasingly operates through theatrical maintenance of this ambiguity (performances of doctrinal coherence, institutional distancing from splinters, splinter counter-narratives of authenticity) rather than through functional resolution. The mandatrophy flag is not raised because the founding problem is demonstrably live (splinters continue to claim it) and the constraint's persistence is explained by genuine disagreement over its resolution, not by institutional inertia or pure theater. However, the rising theater ratio indicates the constraint is approaching the threshold where performance starts to predominate over function — if theater continues rising past 0.70, the mandatrophy signal should be re-examined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'Is the 1890 Manifesto a doctrinal rescission (substitutionist reading) or a prudential suspension under external coercion (continuationist reading)?',
    'Textual analysis of the Manifesto''s own language and institutional pronouncements; comparison with formal doctrinal statements; examination of whether the institution has ever officially repudiated the earlier texts or merely suspended their practice. No single empirical fact resolves this — it is a matter of interpretive framework and authority grounding.',
    'If the Manifesto is rescission, the continuationist doctrine has been officially superseded and splinter communities operate outside the doctrinal tradition. If it is suspension, the continuationist reading remains doctrinally coherent and splinters claim continuity. Classification hinges on whether doctrine is treated as mutable (institutional choice) or immutable (revelation stands regardless of practice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether doctrinal authority is vested in immutable revelation or in institutional pronouncement.').

omega_variable(
    coercion_vs_authentic_development,
    'Does the continuationist framing of the Manifesto as coerced suspension accurately describe institutional motivation, or is it a post-hoc legitimation used by splinters to contest the mainstream''s authority?',
    'Historical evidence from institutional records, private correspondence of leaders, and institutional narratives at the time vs. retrospectively. The question cannot be fully resolved empirically because institutional motivation is counterfactual: would the institution have revised doctrine in the absence of federal coercion? Historical evidence can narrow the question but not eliminate the irreducible uncertainty about counterfactual institutional choice.',
    'If the Manifesto was genuinely coerced (not an authentic doctrinal development), the continuationist reading gains historical grounding and splinters'' authority claim is strengthened. If institutional motivation was mixed or the institution would have moved toward monogamy anyway, the continuationist reading becomes a contested overlay on institutional ambiguity. This affects the degree to which the constraint operates as extraction (using a disputed historical claim to maintain dissent authority) vs. genuine doctrinal dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_authentic_development, empirical, 'The historical motivation for the Manifesto and whether federal coercion was the determinative factor.').

omega_variable(
    women_agency_and_identity_lock,
    'To what extent is the measured suppression in women in polygamous unions structural (legal barriers to exit, economic dependence, geographic isolation, criminal vulnerability) vs. internalized (identity fusion with the religious doctrine, belief in the validity of the arrangement, social isolation from alternative framings)?',
    'Post-exit trajectories: do women who leave polygamous unions experience suppression lift after the structural barriers are removed, or does internalized framing persist? Do they re-enter the belief system or construct alternative meaning-making? The mechanism is observable over time and informative about the nature of suppression.',
    'If suppression is primarily structural, exit interventions (legal protection, economic support, information access) could reduce the constraint''s extractive force. If primarily internalized, the constraint persists in the target''s self-concept even after exit. A high internalized component would indicate the constraint operates through identity fusion (the continuationist reading that polygamy is revelation has been absorbed into the target''s self-understanding). This affects whether the constraint is classified as extractive-with-resistance or extractive-with-captured-targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_agency_and_identity_lock, empirical, 'The composition of suppression mechanisms in the constraint''s operation on women in polygamous unions.').

omega_variable(
    authority_grounding_mutability,
    'Is the principle that ''doctrine is immutable and cannot be rescinded by institutional pronouncement'' itself a settled doctrine within this religious tradition, or is it a contested axiom that different readings interpret differently?',
    'Examination of other instances where the tradition has claimed doctrinal change (e.g., changes in temple rituals, revisions to doctrinal interpretation, new revelations claimed by competing factions). Does the tradition have a consistent principle about doctrinal mutability, or do different parties apply different standards depending on whether they accept the institutional pronouncement in question?',
    'If doctrine-immutability is settled, the continuationist reading has a strong logical foundation and the substitutionist reading must be defended as a special case. If doctrine-mutability is itself contested, both readings are coherent within the tradition and the dispute is over which authority structure (immutability principle vs. institutional pronouncement authority) is supreme. This affects the degree to which the constraint represents a genuine doctrinal dispute vs. a dispute about the rules for settling doctrinal disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_mutability, conceptual, 'Whether the principle of doctrinal immutability is settled or contested within the religious tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.32).
narrative_ontology:measurement_basis(divi_tr_t1890, observed).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.41).
narrative_ontology:measurement_basis(divi_tr_t1920, observed).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.49).
narrative_ontology:measurement_basis(divi_tr_t1950, observed).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement_basis(divi_tr_t1980, observed).
narrative_ontology:measurement(divi_tr_t2010, divine_marriage_command__continuationist_reading, theater_ratio, 2010, 0.57).
narrative_ontology:measurement_basis(divi_tr_t2010, observed).
narrative_ontology:measurement(divi_tr_t2026, divine_marriage_command__continuationist_reading, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(divi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement_basis(divi_be_t1890, observed).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.51).
narrative_ontology:measurement_basis(divi_be_t1920, observed).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement_basis(divi_be_t1950, observed).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(divi_be_t1980, observed).
narrative_ontology:measurement(divi_be_t2010, divine_marriage_command__continuationist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(divi_be_t2010, observed).
narrative_ontology:measurement(divi_be_t2026, divine_marriage_command__continuationist_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(divi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.42).
narrative_ontology:measurement_basis(divi_su_t1890, observed).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement_basis(divi_su_t1920, observed).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.64).
narrative_ontology:measurement_basis(divi_su_t1950, observed).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement_basis(divi_su_t1980, observed).
narrative_ontology:measurement(divi_su_t2010, divine_marriage_command__continuationist_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement_basis(divi_su_t2010, observed).
narrative_ontology:measurement(divi_su_t2026, divine_marriage_command__continuationist_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(divi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel has been decomposed into three constraint stories corresponding to three distinct readings of the 1890 Manifesto. Each reading produces a different ε value (different interpretation of whether the Manifesto represents doctrinal rescission or suspension), different beneficiary/victim structures (different authority grounding), and different classifications (continuationist=tangled_rope due to doctrine-immutability extraction; substitutionist=rope if the new doctrine is accepted as legitimate; coercion_visibility=snare if coercion is treated as illegitimate constraint on doctrine). The three stories are linked via this affects_constraints array and should be analyzed as a constraint family, not as three independent constraints describing the same phenomenon. Decomposition is warranted by ε-invariance: each reading's ε (extractiveness of the doctrine as interpreted) differs significantly based on whether the doctrine is treated as immutable or mutable, and the choice between readings is not observable-dependent but framework-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
