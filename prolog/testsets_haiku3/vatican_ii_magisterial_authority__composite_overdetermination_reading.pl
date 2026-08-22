% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_composite_overdetermination, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority via Composite Overdetermination
 *   domain: institutional/hermeneutical/ecclesiological
 *
 * SUMMARY:
 *   Vatican II (1962–1965) produced conciliar documents that are
 *   simultaneously read as affirming continuity with pre-conciliar tradition
 *   and as authorizing fundamental rupture. This reading does not claim which
 *   reading is correct—that is the sibling readings' territory. Rather, it
 *   asks: what if the texts were designed to be read both ways? Under this
 *   reading, the Council encoded incompatible ecclesiological visions via
 *   deliberately ambiguous formulations to achieve the two-thirds
 *   supermajority required for final passage. The constraint is that parties
 *   are now locked into competing authoritative interpretations of the same
 *   documents, with no neutral arbiter of which interpretation the
 *   magisterium actually endorsed. Hermeneutical control—the power to
 *   determine which reading is 'the' magisterial meaning—becomes the real
 *   locus of authority, and implementation divergence across the
 *   post-conciliar Church is not a failure to enforce uniform doctrine but a
 *   structural feature of the composite texts.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium_interpreters: bishops, theologians, and reform commissions who determine how Council texts are implemented; high institutional power, generational time horizon, mobile exit through alternative reading selection
 *   - continuity_tradition_defenders: bishops and theologians committed to reading Vatican II as organic development; powerful institutional position within pre-conciliar framework, generational time horizon, constrained exit (cannot leave the Church but can resist implementation)
 *   - Council_Fathers_voting_bloc: the bishops whose votes created the supermajority, aware of intentional ambiguity; institutional power, biographical horizon, now deceased or no longer decision-makers—their authority is exercised posthumously through text interpretation
 *   - post_conciliar_magisterium_official: papal authority and curia officials responsible for implementing Council teaching; highest institutional power, generational horizon, arbitrage exit (can declare one reading canonical and end the contest, but costs internal division)
 *   - lay_faithful_and_religious_communities: experience implementation divergence in parish life and community practice; moderate to powerless, biographical to generational horizon, identity-locked (cannot easily exit Catholicism; exit means leaving the tradition itself)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority via Composite Overdetermination").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "institutional/hermeneutical/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '85449f90-75f9-4b08-8500-22656d174b7e').
narrative_ontology:cs_kernel_codification('85449f90-75f9-4b08-8500-22656d174b7e', fixed_text).
narrative_ontology:cs_authority_grounding('85449f90-75f9-4b08-8500-22656d174b7e', extraction).
narrative_ontology:cs_interpretation_layer_present('85449f90-75f9-4b08-8500-22656d174b7e').
narrative_ontology:cs_reading_relation('85449f90-75f9-4b08-8500-22656d174b7e', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('85449f90-75f9-4b08-8500-22656d174b7e', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('85449f90-75f9-4b08-8500-22656d174b7e', foundational, conciliar_texts_intentionally_overdetermined).
narrative_ontology:cs_axiom_status(conciliar_texts_intentionally_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('85449f90-75f9-4b08-8500-22656d174b7e', conciliar_texts_intentionally_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('85449f90-75f9-4b08-8500-22656d174b7e', foundational, hermeneutical_authority_is_real_magisterial_authority).
narrative_ontology:cs_axiom_status(hermeneutical_authority_is_real_magisterial_authority, holdable).
narrative_ontology:cs_axiom_grounding('85449f90-75f9-4b08-8500-22656d174b7e', hermeneutical_authority_is_real_magisterial_authority, deontological).
narrative_ontology:cs_reference_frame('85449f90-75f9-4b08-8500-22656d174b7e', unified_magisterial_teaching).
narrative_ontology:cs_drift_state('85449f90-75f9-4b08-8500-22656d174b7e', contemporary_post_conciliar_pluralism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85449f90-75f9-4b08-8500-22656d174b7e', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, hermeneutical_magisterium_controllers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, continuity_tradition_defenders).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_authority_clarity_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium_and_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, rupture_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, rupture_reform_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, clarity_seekers_and_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_implementation_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the magisterium's official interpretation of Vatican II by way of papal encyclicals, curia responses to dubia, and implementation directives. Benefits from the texts' ambiguity because it permits papal authority to adjudicate disputes between competing reading communities without explicitly choosing sides—if the Pope declares one reading canonical, the other is implicitly delegitimized. Their exit from the constraint would require either revoking Vatican II (impossible without delegitimizing papal authority) or explicitly acknowledging the texts are overdetermined (which would fracture the magisterium's claimed unity). They administer the constraint through silence: they do not acknowledge the overdetermination, they enforce both readings selectively, and they manage contradictions by appeal to context and development of doctrine.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium_and_curia, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, papal_magisterium_and_curia, beneficiary).

% Defend the reading of Vatican II as organic development within unbroken tradition: the Council did not change doctrine, only its expression and pastoral approach. They interpret ambiguous texts through the lens of pre-conciliar teaching and resist rupture readings. They bear the cost of watching implementation diverge from what they see as the texts' true meaning, and they cannot achieve coercive enforcement of their reading (the papal magisterium will not permanently endorse it to the exclusion of the other). Their exit is constrained: they cannot leave the Church, and they cannot force a reinterpretation; their only option is to operate institutional structures (seminaries, religious communities) that implement continuity reading locally, which preserves their tradition at the cost of permanent institutional tension with the post-conciliar mainstream.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, continuity_tradition_defenders, payer,
    powerful, generational, constrained, universal).

% Read Vatican II as authorizing fundamental change: openness to modern scholarship, vernacular liturgy, episcopal collegiality as genuine decentralization, interfaith dialogue, religious freedom without natural-law grounding. They benefit from the texts' ambiguity because they can claim the magisterium now supports their agenda, but they bear the cost of constant defensive interpretation against continuity readers who cite the same texts to opposite conclusions. Their exit is mobile: if the post-conciliar Church moves away from reform, they can leave to join more progressive churches or secular institutions. Within the constraint, they are partly beneficiary (they gain institutional cover for their agenda) and partly payer (they must continually defend their reading without being able to claim it is the obviously correct reading).
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, rupture_reform_advocates, beneficiary,
    powerful, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, rupture_reform_advocates, payer).

% Bishops, theologians, and lay faithful who want the magisterium to say plainly what it teaches on contested matters (divorce and remarriage, contraception, women's ordination, liturgical Latin, episcopal authority). They experience the constraint as institutional noise: they cannot get clear teaching, and different bishops implement Vatican II differently, making it impossible to know what the Church actually requires. Their exit is identity-locked: they fuse their religious identity with Catholicism, and leaving the Church to find clearer teaching means losing their tradition. They are victimized by the constraint's ambiguity—they pay the cost of confusion without gaining the interpretive authority that beneficiaries accrue.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, clarity_seekers_and_faithful, payer,
    moderate, biographical, identity_locked, global).

% Diocesan bishops, religious superiors, and liturgical reform commissions tasked with implementing Vatican II. They bear the cost of acting on ambiguous mandates: they must change some things (vernacular liturgy, ecumenical openness) while preserving others (papal authority, apostolic succession) without clear guidance on which is which. Their exit is constrained: they cannot refuse to implement, and they cannot force a clarification; they can only make local choices (liberal or conservative interpretation) and hope the papal magisterium backs them. Implementation divergence across dioceses and religious communities is a direct result of this constraint's structure: different communities read the same ambiguous texts and implement them incompatibly because the texts authorize both readings.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_implementation_bodies, payer,
    organized, biographical, constrained, national).

% The continuity_reading and rupture_reading communities—they are excluded from the consciousness of institutional magisterial discourse because the magisterium claims a unified teaching. If the papal authority were to acknowledge the composite_overdetermination_reading, it would have to grant that both the continuity and rupture readings were intentional, which would fracture the claim of unified magisterium. They are trapped: they hold competing positions on Vatican II's meaning, and they cannot resolve the contest because both readings are authorized by the same texts, and the texts cannot be revoked or publicly declared ambiguous without delegitimizing magisterial authority.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, sibling_reading_communities, excluded,
    powerful, generational, trapped, universal).

% Historians, theologians, and analysts who examine Vatican II from outside the institutional magisterium's authority structure. They document the voting patterns, trace the schema revisions, and analyze the textual ambiguities without needing to maintain the fiction of a unified magisterium. Their role is to measure the constraint's structure, not to enforce it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II was called to achieve unity in a rapidly modernizing Church by articulating a conciliar response to contemporary challenges (scientific method, religious pluralism, state-church relations, liturgical renewal). The coordination problem was: how can a traditionalist institution remain unified while responding to modern forces? The Council's answer was to encode both conservative and progressive readings into ambiguous texts, permitting each camp to claim the magisterium's support. The texts coordinate by deferring to hermeneutical interpretation rather than by specifying doctrine—instead of saying 'the Church now permits or forbids X,' the texts say 'the Church respects X and also affirms Y,' and subsequent readers fill in whether X and Y are compatible.
% TRANSFER_FUNCTION: The constraint moves interpretive authority from the conciliar texts themselves (which would have to be read univocally) to the post-conciliar magisterium (which can adjudicate readings). It also moves institutional power from local bishops (who might act on their own reading) to the papal center and reform commissions (who control the hermeneutical narrative). Doctrinal clarity is transferred from the texts to the reading community—no one now doubts what Vatican II says, but everyone disputes what it means, and the meaning is determined by who can claim magisterial backing for their interpretation.
% ABSENT_VOICES: Voices calling for explicit acknowledgment that Vatican II encodes incompatible visions are structurally absent from official magisterial discourse. Reformers who want to admit that Vatican II represents rupture with pre-conciliar teaching, and traditionalists who want to admit that Vatican II requires choosing between continuity and adaptation—both are excluded because admitting overdetermination would fracture the magisterium's claim of unified teaching. Progressive theologians who want to say 'Vatican II means what I say it means, not what traditionalists say' are also absent—they instead claim 'Vatican II teaches this, and traditionalists are wrong to read it differently,' which maintains the fiction of univocal meaning while fighting for interpretive dominance. The constraint is maintained by this silence: once voiced, the composite reading becomes impossible to suppress, and the magisterium's authority dissolves into competing hermeneutical camps.
% DISAPPEARANCE_RATIONALE: If the composite_overdetermination_reading disappeared as an operative constraint—if the magisterium acknowledged that Vatican II encodes incompatible visions and explicitly chose one reading (continuity or rupture) as canonical—the Church would rearrange fundamentally. Continuity readers would either accept the rupture reading and reform, or they would separate (creating a traditionalist schism). Rupture advocates would either accept continuity and slow down reform, or they would depart for more progressive churches. Clarity seekers would finally get answers, though those answers would trigger departures. The post-conciliar Church's current institutional peace depends on all parties believing the magisterium is unified and on their own reading. Remove that fiction, and the Church fractures into its component parts.
% FOUNDING_PROBLEM: Unity-in-change: the Council was called in 1958 to renew the Church and make it accessible to the modern world without losing its traditions. The founding problem was: how can a two-thousand-year-old institution remain unified while adapting to postwar pluralism, scientific discovery, and the erosion of Christendom in the West?
% FOUNDING_PROBLEM_CORROBORATION: The papal magisterium and reform advocates attest the founding problem is still live—the post-conciliar Church continues to face the tension between traditionalists and reformers, and Vatican II's ambiguous texts permit the magisterium to manage this ongoing tension. Continuity defenders attest the founding problem is dead—the Council was not meant to solve it but to deny it was real; the real problem was ecclesiastical pride and refusal to accept that tradition is sufficient. Independent historians and theologians attest the founding problem is genuinely contested: the Council's documents show both groups claiming victory, and the texts contain genuinely unresolved theological positions (e.g., the eucharist as meal vs. sacrifice, the bishop as successor of apostles vs. agent of papal delegation). No independent voice external to all parties claims the founding problem is simply solved.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.62 over the measurement interval (0–10 years post-conciliar, roughly 1965–1975) as hermeneutical battles move from the Council chamber to implementation. Early extractiveness is lower because the constraint's overdetermination is not yet contested—the texts are fresh, and the initial assumption is that there is one magisterial meaning to be discovered. As implementation diverges and different parties cite the same texts in support of opposite positions (liturgical Latin vs. vernacular; episcopal collegiality as check on papal authority vs. papal authority as interpreter of collegiality), the extractiveness rises: those who can impose readings gain advantage over those bound by readings they reject. Theater rises from 0.22 to 0.48 as the constraint requires increasing institutional performance—defending one reading while acknowledging that the texts support others, maintaining unity while permitting radical implementation divergence, performing doctrinal continuity while executing doctrinal change. Suppression requirement rises modestly (0.42 to 0.58) because the constraint's persistence depends on preventing explicit acknowledgment that the texts are overdetermined: if the Council said the same thing both ways intentionally, the pretense of a unified magisterium collapses. The suppression is not coercive violence but institutional silence on the composite character of the texts. Accessibility collapse is high (0.71) because once one grasps the overdetermination reading, alternatives (naive unified-meaning reading, simple continuity or rupture without composite framing) become cognitively accessible but institutionally unavailable—the constraint is 'you may read these texts as you wish, but you may not say they were composed to permit you to read them as you wish.' Resistance is high (0.69) because continuity defenders and rupture advocates both reject the composite reading: continuity defenders see it as undermining the magisterium's authority; rupture advocates see it as a cop-out that avoids honest acknowledgment of change.
 *
 * PERSPECTIVAL GAP:
 *   From the papal curia's seat, Vatican II is a coordinated, unified magisterial act: the Council achieved a supermajority on carefully balanced texts, and subsequent implementation is the faithful unfolding of that teaching. Apparent divergence reflects legitimate pluralism in applying universal principles to diverse local circumstances. From the continuity defender's seat, Vatican II contains seeds of rupture that a less-careful reading-community would have avoided—the real magisterium is the pre-conciliar teaching, and the post-conciliar magisterium should be read through it, not vice versa. From the rupture advocate's seat, Vatican II is the Church finally catching up to modernity, and the continuity defenders are holding back a genuine change that the texts clearly authorize. From the composite overdetermination reading—this one—Vatican II is none of these. It is a structure in which the magisterium encoded incompatible visions to achieve supermajority passage, and now no seat can claim the texts mean what that seat wants without implicitly acknowledging that they also mean what the opposing seat wants. The constraint is the shared ceiling: all parties are bound to treat the texts as authoritative, and none can escape by pointing to the texts' ambiguity without delegitimizing the magisterium's authority. This reading predicts that the Church will never achieve settled consensus on Vatican II's meaning—not because the reading community is divided (they are), but because the texts were designed to be divided.
 *
 * DIRECTIONALITY LOGIC:
 *   Hermeneutical magisterium controllers (papal curia, reform commission members, influential theologians with interpretive authority) are net beneficiaries: they gain the power to determine which reading is canonical, and that power flows from the texts' ambiguity—if the texts were unambiguous, interpretation would be constrained. Their directionality is near beneficiary (d ~ 0.20–0.35): they do not directly collect rents, but they accrue interpretive authority and the ability to resolve ambiguity in their favor. Post-conciliar reform advocates (bishops and theologians implementing Vatican II as rupture, opening the Church to modern scholarship and practice) are partially beneficiary, partially dependent on the hermeneutical controllers' willingness to permit their reading—they benefit from the texts' ambiguity but are constrained by the need to maintain that the magisterium is unified. Continuity tradition defenders are net payers: they are locked into defending a reading that the texts support only ambiguously, while the opposing reading gains institutional momentum. Their exit is constrained (trapped: they cannot leave the Church) or identity-locked (they fuse their religious identity with the continuity reading and cannot psychologically defect even if institutionally permitted). Clarity seekers (bishops and faithful who want the magisterium to simply say what it means) are victimized by the constraint—they are unable to get a unified, unambiguous pronouncement because doing so would require denying the composite character and choosing a side, which the institutional magisterium cannot do without fracturing the post-conciliar consensus. Their directionality is near target (d ~ 0.75–0.85): they bear the cost of ambiguity (confusion, divergent pastoral practice, inability to trust magisterial pronouncements) without gaining the interpretive authority that beneficiaries accrue.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—unity in a rapidly modernizing Church—is contested. The papacy and reform advocates argue the founding problem is still live: the post-conciliar Church faces the same challenge of unity-in-change that called the Council. Continuity defenders argue the founding problem was solved in reverse: the real challenge was to change without rupturing, and Vatican II failed. The composite overdetermination reading does not adjudicate this—it claims instead that the texts embed the contest structurally. The constraint persists not because the founding problem is live, but because all parties are locked into treating the texts as authoritative even while disagreeing radically on what they authorize. The magisterium cannot revoke the texts without admitting the Council was fallible; continuity defenders cannot enforce a single reading without acquiring coercive power they do not possess; rupture advocates cannot openly declare victory without triggering institutional schism. The constraint's persistence is the cost of maintaining the fiction of unified magisterium on documents that were designed to be read incompatibly. Mandatrophy is present but unresolved: the founding problem (unity-in-change) is dead if the Church has achieved operational consensus despite textual ambiguity (the reform advocates' case), or live if the Church is still tearing itself apart over Vatican II's meaning (the continuity defenders' case). The two-thirds supermajority votes (88%, with 10–12% rejections) on key schema suggest unresolved incompatibility was embedded by design, not discovered later—mandatrophy is a deliberate, structural feature, not an accident.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_compromise_encoding,
    'Did the Council Fathers deliberately craft ambiguous formulations to achieve supermajority votes, or did ambiguity arise from genuine theological difference that the texts failed to resolve?',
    'Historical analysis of voting patterns, Council diaries (Acta), interventions during schema revisions, and comparisons between early drafts and final texts: if high-conflict schema iterations show systematic movement toward ambiguity as voting thresholds approached, intentional compromise is evidenced; if ambiguity persists across all drafts, it reflects unresolved theology rather than strategic encoding.',
    'Intentional compromise reading supports this constraint''s claim: authority structure is overdetermined by design, and hermeneutical control becomes the real exerciser of magisterial power. Unresolved-theology reading flattens the claim toward simple rupture/continuity ambiguity rather than structural overdetermination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_compromise_encoding, empirical, 'Whether textual ambiguity was strategic (to achieve consensus) or substantive (unresolved theological difference).').

omega_variable(
    hermeneutical_authority_locus,
    'After Vatican II, who holds the authority to determine which reading of the texts is the authoritative magisterial meaning—the text as written, the implementing body, the post-conciliar magisterium, or the scholarly interpretive tradition?',
    'Analysis of papal and episcopal pronouncements on contested Council documents (e.g., competing claims about Sacrosanctum Concilium, Dignitatis Humanae, Ad Gentes); if different authorities cite the SAME text to support opposite conclusions and neither authority revokes the other''s claim, the locus of definitional authority is genuinely contested and distributed rather than centralized.',
    'If hermeneutical authority is distributed/contested, the constraint is indeed tangled_rope: apparent coordination (unified conciliar teaching) masks asymmetric extraction (those who can impose interpretations gain magisterial advantage over those bound by texts they reject). If authority is consolidated in the papacy or a specific body, the constraint becomes simpler snare (enforced reading despite ambiguity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_authority_locus, conceptual, 'Whether hermeneutical authority over the texts is centralized or distributed.').

omega_variable(
    supermajority_voting_signal,
    'Do the 10-12% minority rejection votes on key schema represent residual theological incompatibility embedded in final texts, or merely procedural dissent that was resolved in the final vote?',
    'Cross-reference voting records with subsequent theological positions taken by rejecting bishops and theologians: if rejecting parties later defend alternative readings that the texts support, the embedded incompatibility reading is corroborated; if rejecting parties accept the final formulations despite their votes, the votes represent dissent that was procedurally overridden, not structural incompatibility.',
    'Embedded incompatibility supports composite overdetermination: the texts were knowingly voted on with unresolved internal contradiction. Procedural dissent supports continuity/rupture ambiguity as simple negotiation outcome rather than structural overdetermination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_voting_signal, empirical, 'Whether minority votes signal embedded theological incompatibility.').

omega_variable(
    implementation_divergence_as_feature,
    'Is the radical divergence in post-conciliar implementation across dioceses and religious communities a bug (failed enforcement) or a feature (structural consequence of overdetermined texts)?',
    'Comparative analysis: if implementation patterns correlate with which reading of ambiguous texts a bishop or community endorsed (continuity readers implement cautiously, rupture readers implement expansively), divergence is structural and predicted by the texts'' overdetermination. If divergence is random or driven by local factors unrelated to reading choice, it is a feature of weak enforcement rather than text design.',
    'Structural feature reading supports composite overdetermination: the texts were designed to permit incompatible implementations. Weak-enforcement reading supports simple rupture/continuity ambiguity as a textual confusion rather than intentional composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_divergence_as_feature, empirical, 'Whether implementation divergence follows from textual overdetermination or from enforcement failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(vati_tr_t2, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(vati_tr_t4, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 6, 0.41).
narrative_ontology:measurement(vati_tr_t8, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vati_be_t2, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(vati_be_t4, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(vati_be_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(vati_be_t8, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(vati_su_t2, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(vati_su_t4, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(vati_su_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(vati_su_t8, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_magisterial_authority kernel has three constraint readings: continuity_reading (Vatican II is organic development, low extraction, mountain/rope type), rupture_reading (Vatican II breaks with prior teaching, higher extraction, tangled_rope/snare type), and this composite_overdetermination_reading (the texts were designed ambiguously to achieve supermajority passage, extractiveness from hermeneutical control). The three readings are not alternative ways to view the same constraint—they are three structurally distinct constraints arising from one contested kernel. Each has its own epsilon, its own beneficiaries/victims, its own stakeholders, and its own classification. The network edges link them: composite reading affects both continuity and rupture readings because the composite framing changes what each reading means (if texts are overdetermined, continuity defenders cannot claim naive unambiguity, and rupture advocates cannot claim simple rupture—both are forced into the position of 'my reading is the correct reading of an intentionally ambiguous text'). This constraint family illustrates the ε-invariance principle: the three readings have measurably different epsilons (continuity is lowest extraction from genuine coordination; rupture is higher from enforced change; composite is higher still from hermeneutical control locus), and the differences are not observer-relative measurement choices—they are structural facts about what each reading asserts the constraint IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
