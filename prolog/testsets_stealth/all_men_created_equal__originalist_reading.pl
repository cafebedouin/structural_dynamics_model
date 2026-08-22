% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading: Equality of the Declaration Bounded by Founding-Era Social Taxonomy
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Declaration's equality clause is a contested kernel; this file
 *   instantiates ONE reading of it — the originalist reading, on which the
 *   principle's scope is fixed by the founding generation's understanding and
 *   is therefore bounded by the 18th-century social taxonomy of political
 *   membership. The standing arrangement under contest — the referent of
 *   epsilon — is that taxonomy-bounded regime as it has actually operated
 *   from 1776 to the present, assessed honestly rather than by the reading's
 *   self-description; the reading's endorsed alternative (or its siblings'
 *   alternatives) is never the referent. Sibling readings are separate
 *   constraints in separate files; the contest between readings is recorded
 *   in the omega variables, not averaged into this one. The claim/metrics gap
 *   is deliberate and independent: the constraint is CLAIMED as tangled_rope
 *   — a genuine interpretive coordination function carrying asymmetric
 *   extraction through the same scope rule — while the authored metrics
 *   describe extraction that peaked near-maximal in the Dred Scott era,
 *   collapsed with the Fourteenth Amendment, partially revived under Jim
 *   Crow, declined after Brown, and is rising again under the current Court's
 *   originalist ascendancy. The engine computes per-seat types from the
 *   structural data; the authored claim does not adjudicate them.
 *
 * KEY AGENTS:
 *   - slaveholding_class: historical primary beneficiary (powerful/arbitrage) — the taxonomy reading secured its labor regime; the exit actually exercised was secession
 *   - founding_elite_descendants: standing beneficiary (powerful/mobile) — inherited status allocations shielded from reinterpretation, silently
 *   - originalist_legal_movement: beneficiary and operational custodian (institutional/mobile) — converts the reading into authority, appointments, and career capital
 *   - enslaved_people_and_descendants: primary target across the interval (powerless/trapped) — bears the scope restriction's costs in full
 *   - women_denied_civic_standing: target via coverture and the 1875 suffrage ruling (powerless/trapped)
 *   - indigenous_nations: target — excluded from 'men' while subject to federal power (powerless/trapped, continental scope)
 *   - non_european_immigrants: target — the taxonomy's racial boundary administered at the point of entry (powerless/constrained)
 *   - supreme_court: agenda-setter (institutional/constrained) — its composition determines the operative reading; oscillates between readings as appointments turn over
 *   - abolitionist_and_suffragist_movements: excluded voices — objected from outside the founding settlement and were answered with the scope restriction itself (organized/trapped)
 *   - constitutional_historians: analytical observer — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.6).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.45).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading: Equality of the Declaration Bounded by Founding-Era Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'cf65bf5a-13aa-44eb-8d15-ea86380cfdb1').
narrative_ontology:cs_kernel_codification('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', fixed_text).
narrative_ontology:cs_authority_grounding('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', lineage).
narrative_ontology:cs_interpretation_layer_present('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1').
narrative_ontology:cs_reading_relation('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', foundational, founders_intent_governs_equality_scope).
narrative_ontology:cs_axiom_status(founders_intent_governs_equality_scope, holdable).
narrative_ontology:cs_axiom_grounding('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', founders_intent_governs_equality_scope, conventional).
narrative_ontology:cs_axiom('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', foundational, founding_equality_referents_taxonomy_bounded).
narrative_ontology:cs_axiom_status(founding_equality_referents_taxonomy_bounded, holdable).
narrative_ontology:cs_axiom_grounding('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', founding_equality_referents_taxonomy_bounded, empirically_contingent).
narrative_ontology:cs_reference_frame('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', founding_era_public_meaning).
narrative_ontology:cs_drift_state('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', contemporary_doctrine, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cf65bf5a-13aa-44eb-8d15-ea86380cfdb1', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, slaveholding_class).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_people_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_denied_civic_standing).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_european_immigrants).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pre-1865 planter class of the South. The founding-era scope of the equality declaration kept enslaved people outside its protection, which secured the labor regime this class depended on. Its members held the political power to defend that scope in Congress and the courts, and when the scope was threatened by an ascending anti-slavery politics, the option actually exercised was secession and war. The class was destroyed in 1865.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, slaveholding_class, beneficiary,
    powerful, generational, arbitrage, regional).

% Inheritors of the social and economic positions the founding settlement allocated. They take no action and bear no burden: the reading works for them silently, by ruling out of scope the equality claims that would unsettle inherited position. The benefit persists whether or not anyone advocates for it, and exit is irrelevant because nothing constrains them.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    powerful, generational, mobile, national).

% The network of jurists, law professors, advocacy organizations, and clerkship pipelines that produces the historical scholarship operationalizing 'founders' intent' and staffs the judiciary that applies it. Each doctrinal victory for the reading converts into appointments, authority, and career capital for its members; the movement's professional identity is fused with the method it administers.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_movement, beneficiary,
    institutional, generational, mobile, national).

% The group that bore the reading's costs across the whole interval. The scope restriction placed them outside the declaration's protection while slave codes, and later Black codes and Jim Crow statutes, governed their lives. Exit meant flight, the Underground Railroad, or rebellion at mortal risk. Their descendants litigate equality claims today that the reading scopes down wherever founders'-intent arguments prevail.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_people_and_descendants, payer,
    powerless, generational, trapped, national).

% Women under coverture and disenfranchisement, placed outside the taxonomy's political membership at the founding. The reading was applied against them directly when the Court rejected suffrage claims on founders'-intent grounds in 1875. Their route out ran through constitutional amendment rather than interpretation — a campaign of more than seventy years for the vote, and longer for equal civic standing.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_denied_civic_standing, payer,
    powerless, generational, trapped, national).

% Native nations governed by federal power while excluded from the declaration's 'men'. The scope restriction denied them equal standing while treaties were broken and land cessions proceeded; their citizenship arrived only by statute in 1924. The exit actually administered to many was removal — forced relocation rather than departure by choice.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    powerless, generational, trapped, continental).

% Immigrants whose admission to equal standing was decided by the taxonomy's racial boundary at the point of entry: exclusion acts barred entry by nationality, and the naturalization cases showed the boundary administered case by case through racial classification. Exit was not available to them — the boundary determined who could be inside at all.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_european_immigrants, payer,
    powerless, biographical, constrained, national).

% The institution that decides which reading of the equality declaration governs. Its composition determines the operative reading: one Court enforced the taxonomy reading at its extreme in 1857, a later Court repudiated it in 1954, and the current majority is rebuilding scope reasoning on founders'-intent foundations. The Court cannot leave its role — some reading must govern — but its commitments swing between readings as appointments turn over.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% The organized voices the founding settlement excluded from the conversation that fixed the scope. They objected from outside — the 1852 address asking what the declaration's promise meant to the enslaved is the canonical statement of the objection — and were answered with the scope restriction itself. They gained standing inside the interpretive conversation only through mass mobilization and amendment.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, abolitionist_and_suffragist_movements, excluded,
    organized, generational, trapped, national).

% Scholars who reconstruct founding-era understanding and trace the phrase's contested career across two and a half centuries. Their findings discipline both the originalist and universalist readings and supply the evidence base the paradox reading works from. They collect nothing from how the reading operates.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a determinate rule for the scope of the constitutional equality guarantee: what the enacted text requires is settled by founding-era understanding rather than case-by-case moral revision, letting courts, legislatures, and citizens coordinate expectations. It also coordinates the legal profession around a shared method — historical reconstruction — for resolving the clause's application.
% TRANSFER_FUNCTION: Moves civic standing and legal protection from those outside the founding taxonomy (enslaved people and their descendants, women, indigenous nations, non-European immigrants) to those inside it. The scope restriction converts their exclusion into preserved status allocations and, for the reading's professional custodians, into interpretive authority and judicial appointments.
% ABSENT_VOICES: The enslaved, women, and indigenous nations were absent from the founding conversation that fixed the reading's scope. Abolitionists and suffragists objected from outside it and were answered with the scope restriction itself. Contemporary descendants of excluded groups enter only as litigants, before an agenda-setting seat that the reading's beneficiaries helped staff through the appointment pipeline.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight — if courts stopped asking what the founding generation meant and treated the principle as universal — the scope-limited line of doctrine would lose its methodological foundation, pending and decided cases resting on founders'-intent reasoning would be reopened, the originalist movement would lose its central equality case, and the status settlements the scope protects would be exposed to revision. The universalist reading would not simply fill the vacancy; the whole interpretive settlement would have to renegotiate.
% FOUNDING_PROBLEM: Holding a revolutionary coalition together: the Declaration proclaimed a universal-sounding equality while the founding states' law and economy rested on chattel slavery, coverture, and dispossession. The taxonomy-bounded reading solved that contradiction — proclaim the principle, bind its referents to the existing social taxonomy — so that slaveholding and propertied delegates could sign.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Frederick Douglass's 1852 address attacked precisely the taxonomy reading; the Reconstruction framers' debates record the formal repudiation of the founding scope; and constitutional historiography independent of the originalist movement documents the scope restriction as a coalition-maintenance device. No source outside the originalist legal movement attests that the founding coalition's problem remains live — the movement's own 'fidelity to enacted meaning' testimony is a beneficiary attestation, not corroboration.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.6 (present) against a series that peaked at 0.92 in 1857, when the reading denied any rights to the enslaved by name. The present value is lower than the peak but still substantially extractive: the reading is institutionally ascendant and actively strips scope from equality claims (the colorblind-originalist line of cases). Suppression (0.45, raw and unscaled — only extractiveness is scaled by directionality and scope) tracks the enforcement machinery's history: slave codes and patrols, then Jim Crow enforcement, then a low plateau of professional gatekeeping, now rising again through confirmation politics. Theater (0.4) is real but secondary — the movement's historical scholarship is genuine work; the theatrical share is selective historicism, where founding reverence is invoked when convenient and the founding's own contradictions are absorbed by the interpretive layer rather than surfaced. Accessibility_collapse is low (0.4): understanding the reading does not collapse alternatives — the universalist reading, the amendment route, and the paradox critique all remain fully available, which is why resistance is very high (0.85): abolition, civil war, suffrage, the civil rights movement, and continuing litigation are the constraint meeting organized coalition resistance from the very groups it scopes out. The temporal series runs on one shared nine-point grid so every metric is authored at every examined time point; no suppression_requirement drift claim is made beyond what the enforcement record shows.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute differently. From the originalist_legal_movement seat, the arrangement is a scholarly discipline it built and staffs — the reading is experienced as method, not extraction, and the movement's identity is fused with it (institutional identity lock: the movement has 'become' its method; if that frame broke, the movement would not exit the constraint, it would dissolve as a movement). From the trapped payer seats, the same scope rule operated as the thing that placed them outside the declaration's protection. Same-power differentiation among the powerless is constraint-specific: enslaved people and women both sat at the powerless atom, but exit differed structurally — chattel bondage versus coverture — and indigenous nations faced removal rather than exit at continental scope. The supreme_court seat flips with composition: the same institution enforced this reading in 1857, repudiated it in 1954, and is rebuilding it now; the agenda-setter's directionality toward the constraint is a function of appointment politics, not of the constraint's content.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: slaveholding_class was the historical full beneficiary (d near 0.0, now defunct as a seat); founding_elite_descendants hold near-beneficiary position with diffuse, silent gain; originalist_legal_movement is a declared beneficiary with an administrative overlay — the derivation places it low-d, which is correct for its benefit, while its enforcement role is captured by the agenda_setter seat it partially staffs. The victim declarations map to high directionality, and the trapped exit atoms of the three historical victim seats push them toward the full-target end; non_european_immigrants' constrained (rather than trapped) exit moderates this slightly. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already encode the asymmetry the derivation needs, and the one genuinely ambiguous seat (the Court, an institutional administrator with no declared beneficiary or victim position) is left to the canonical fallback with its oscillation documented in commentary rather than frozen by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding a slaveholding-and-propertied coalition together under a universal-sounding equality declaration — is dead: it dissolved with the destruction of slavery and the formal repudiation of the taxonomy. The reading persists anyway, sustained by new beneficiaries (the legal movement's professional economy) and by silently preserved status allocations. That is exactly the R5 mismatch signature (founding_problem_status=dead + disappearance_verdict=world_rearranges), which should flag capture/zombie persistence and route cross-checking against the theater and piton paths. Classifying the reading as tangled_rope rather than snare preserves what is genuinely still coordinated — determinate interpretive method is a real coordination good the profession consumes daily — while the victim declarations and the temporal series keep the extraction visible; classifying it as rope would erase the excluded groups who paid, and as snare would overstate the case, since the coordination function is not mere cover. The reading is a live candidate for mandatrophy resolution in its founding function even as its secondary function (movement authority) remains fully operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel all_men_created_equal — the originalist_reading. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the kernel family: author and classify all_men_created_equal__universalist_reading and all_men_created_equal__textualist_paradox_reading with the same referent discipline, then compare beneficiary/victim structure, epsilon, and type across the three. The disagreement''s location is the scope-determination rule: founders'' intent (this reading) versus the principle''s own universal logic (universalist) versus the text''s internal performative contradiction (paradox).',
    'If the universalist reading proves to be the structurally dominant instantiation, this reading''s extraction registers as a contested minority position rather than the operative constraint. If the paradox reading is right that no coherent founders'' intent existed, this reading''s coordination function collapses and its type shifts toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; contest located at the scope-determination rule.').

omega_variable(
    founding_meaning_empirical_dispute,
    'What did the founding generation actually understand ''all men are created equal'' to cover — a claim bounded by the era''s social taxonomy, an anti-slavery principle already in tension with practice, or an aspirational statement without operative scope?',
    'Historiographic and archival convergence independent of movement scholarship: state-formation records, ratification debates, and abolition-era usage of the phrase, weighed by constitutional historians outside the beneficiary set.',
    'If the founders meant an anti-slavery principle, the reading''s empirical axiom fails and it dissolves toward universalism. If the taxonomy reading is historically accurate, the paradox reading sharpens. If the phrase was aspirational-only, the reading''s coordination function was never genuine and the structure is cover from its founding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_meaning_empirical_dispute, empirical, 'The empirical content of the reading''s foundational historical axiom is itself contested by historians.').

omega_variable(
    frame_shift_1868_status,
    'Does the reading''s reference frame include the Fourteenth Amendment''s ratification-era public meaning (1868) as the governing anchor, or only 1776/1789? The reading survived mid-century repudiation partly by shifting its frame to 1868.',
    'Doctrine and movement texts: whether originalist equality arguments cite 1789/1791 understanding or 1868 ratification history, and whether the movement treats the frame shift as a legitimate refinement or a concession extracted under pressure.',
    'A 1868-anchored frame yields different scope outcomes (it can accommodate Brown and narrower costs for excluded groups) than a founding-anchored frame; the reading''s current extraction level depends materially on which anchor the operative doctrine uses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frame_shift_1868_status, conceptual, 'Internal frame ambiguity: which founding moment anchors ''founders'' intent'' for the equality clause.').

omega_variable(
    professional_socialization_suppression,
    'Is the suppression that sustains this reading structural (appointment control, doctrinal gatekeeping, confirmation politics) or internalized (professional socialization that makes founders''-intent the only legitimate question to ask), and in what proportion?',
    'Post-capture suppression trajectory: if law schools and courts trained under the movement revert to pluralist interpretive method when appointment control shifts, the suppression was structural; if the method persists as professional common sense after its enforcement machinery weakens, part of the suppression is internalized.',
    'Internalized suppression would raise the reading''s effective suppression above the structural measure and slow any reversal of its extraction even after the agenda-setting seat changes composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_socialization_suppression, empirical, 'Structural versus internalized suppression in the reading''s professional enforcement machinery.').

omega_variable(
    descendant_benefit_attribution,
    'Do the gains of the reading''s contemporary operation actually accrue to founding_elite_descendants (silently preserved status allocations) or to originalist_legal_movement (authority, appointments, career capital), and is the descendants'' benefit real or merely residual?',
    'Trace each scope-restricting doctrinal victory to its beneficiaries: who gains standing, policy, or capital from the decision, compared against status-allocation effects that operate without any actor''s action.',
    'If the movement captures the gains, the receipt seat shifts to the movement and the reading''s contemporary form is closer to professional rent collection than status protection; if the descendants accrue, the extraction preserves the founding settlement''s hierarchy directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(descendant_benefit_attribution, empirical, 'Receipt attribution between the two living beneficiary seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amce_originalist_tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.2).
narrative_ontology:measurement(amce_originalist_tr_t1808, all_men_created_equal__originalist_reading, theater_ratio, 1808, 0.25).
narrative_ontology:measurement(amce_originalist_tr_t1857, all_men_created_equal__originalist_reading, theater_ratio, 1857, 0.3).
narrative_ontology:measurement(amce_originalist_tr_t1868, all_men_created_equal__originalist_reading, theater_ratio, 1868, 0.25).
narrative_ontology:measurement(amce_originalist_tr_t1896, all_men_created_equal__originalist_reading, theater_ratio, 1896, 0.4).
narrative_ontology:measurement(amce_originalist_tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.35).
narrative_ontology:measurement(amce_originalist_tr_t1971, all_men_created_equal__originalist_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement(amce_originalist_tr_t2000, all_men_created_equal__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(amce_originalist_tr_t2026, all_men_created_equal__originalist_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(amce_originalist_be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.85).
narrative_ontology:measurement(amce_originalist_be_t1808, all_men_created_equal__originalist_reading, base_extractiveness, 1808, 0.88).
narrative_ontology:measurement(amce_originalist_be_t1857, all_men_created_equal__originalist_reading, base_extractiveness, 1857, 0.92).
narrative_ontology:measurement(amce_originalist_be_t1868, all_men_created_equal__originalist_reading, base_extractiveness, 1868, 0.5).
narrative_ontology:measurement(amce_originalist_be_t1896, all_men_created_equal__originalist_reading, base_extractiveness, 1896, 0.68).
narrative_ontology:measurement(amce_originalist_be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.48).
narrative_ontology:measurement(amce_originalist_be_t1971, all_men_created_equal__originalist_reading, base_extractiveness, 1971, 0.44).
narrative_ontology:measurement(amce_originalist_be_t2000, all_men_created_equal__originalist_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(amce_originalist_be_t2026, all_men_created_equal__originalist_reading, base_extractiveness, 2026, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(amce_originalist_su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.7).
narrative_ontology:measurement(amce_originalist_su_t1808, all_men_created_equal__originalist_reading, suppression_requirement, 1808, 0.75).
narrative_ontology:measurement(amce_originalist_su_t1857, all_men_created_equal__originalist_reading, suppression_requirement, 1857, 0.8).
narrative_ontology:measurement(amce_originalist_su_t1868, all_men_created_equal__originalist_reading, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement(amce_originalist_su_t1896, all_men_created_equal__originalist_reading, suppression_requirement, 1896, 0.65).
narrative_ontology:measurement(amce_originalist_su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(amce_originalist_su_t1971, all_men_created_equal__originalist_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement(amce_originalist_su_t2000, all_men_created_equal__originalist_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(amce_originalist_su_t2026, all_men_created_equal__originalist_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'all men are created equal' covers three structurally distinct constraints (per the epsilon-invariance principle): the originalist reading (this file — scope fixed by founding-era understanding, hence taxonomy-bounded), the universalist reading (equality as universal principle requiring iterative expansion regardless of founder intent), and the textualist paradox reading (the universal language's irreconcilability with restricted application as performative contradiction). Each gets its own epsilon, beneficiaries, victims, and type; this file links to both siblings. The upstream/downstream structure runs through the shared historical evidence base: the better the originalist historical case, the sharper the paradox reading's critique, and the originalist reading's institutional victories change the universalist reading's operating environment without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
