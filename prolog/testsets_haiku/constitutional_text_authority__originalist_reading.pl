% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The originalist reading of constitutional authority claims that the
 *   meaning of the Constitution is fixed at the moment of ratification, and
 *   that judicial interpretation must recover the historical public
 *   understanding of the text as understood by ratifying conventions. This
 *   reading generates a stringent constraint on judicial discretion: judges
 *   cannot recognize unenumerated rights, cannot apply ancient principles to
 *   modern circumstances if the Framers did not explicitly contemplate the
 *   application, and cannot update constitutional meaning based on evolved
 *   moral understanding—only through Article V amendment can meaning formally
 *   change. The constraint operates as tangled rope: it solves a real
 *   coordination problem (stabilizing constitutional meaning across time and
 *   competing interpretations) while extracting substantial costs from rights
 *   claimants and moral reformers whose positions postdate or lie outside the
 *   Framing-era historical record. The originalist reading is ONE reading of
 *   the contested kernel of constitutional authority; the sibling readings
 *   (living constitutionalism, constitutional positivism) instantiate
 *   different constraints from the same foundational commitment.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: Institutional agenda-setter enforcing the fixed-meaning constraint through judicial opinions and doctrine.
 *   - living_constitutionalist_judges: Institutional payers constrained by originalist doctrine from adopting evolutionary interpretation.
 *   - rights_claimants_unenumerated_status: Powerless victims whose moral claims are gatekept by historical evidence requirements.
 *   - contemporary_moral_reformers: Organized payers forced to pursue expensive Article V amendment or historical reinterpretation.
 *   - constitutional_amendment_gatekeepers: Institutional beneficiaries whose monopoly on change is enforced by the constraint.
 *   - historical_evidence_specialists: Moderate beneficiaries whose expertise becomes gatekeeping authority.
 *   - post_ratification_social_movements: Excluded from originalist constitutional argument by temporal structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.62).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.71).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'a15304a2-9be6-43a9-9f04-fb0560a582cb').
narrative_ontology:cs_kernel_codification('a15304a2-9be6-43a9-9f04-fb0560a582cb', fixed_text).
narrative_ontology:cs_authority_grounding('a15304a2-9be6-43a9-9f04-fb0560a582cb', lineage).
narrative_ontology:cs_interpretation_layer_present('a15304a2-9be6-43a9-9f04-fb0560a582cb').
narrative_ontology:cs_reading_relation('a15304a2-9be6-43a9-9f04-fb0560a582cb', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a15304a2-9be6-43a9-9f04-fb0560a582cb', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('a15304a2-9be6-43a9-9f04-fb0560a582cb', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('a15304a2-9be6-43a9-9f04-fb0560a582cb', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('a15304a2-9be6-43a9-9f04-fb0560a582cb', foundational, historical_public_understanding_authoritative).
narrative_ontology:cs_axiom_status(historical_public_understanding_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('a15304a2-9be6-43a9-9f04-fb0560a582cb', historical_public_understanding_authoritative, empirically_contingent).
narrative_ontology:cs_reference_frame('a15304a2-9be6-43a9-9f04-fb0560a582cb', framers_original_public_understanding).
narrative_ontology:cs_drift_state('a15304a2-9be6-43a9-9f04-fb0560a582cb', contemporary_post_warren_court_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a15304a2-9be6-43a9-9f04-fb0560a582cb', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, textual_literalist_legal_tradition).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, rights_claimants_unenumerated_status).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, contemporary_moral_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, constitutional_amendment_gatekeepers).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, historical_evidence_specialists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges (particularly appellate and Supreme Court) who adopt originalism as the binding interpretive methodology. They set the rule that constitutional meaning is fixed at ratification and enforce it by rejecting claims of unenumerated rights and refusing to update meaning based on contemporary values. They control the authoritative legal interpretation of the Constitution through judicial opinions and maintain originalist doctrine through successive cohorts of judges.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Judges who believe constitutional meaning evolves with society and want to recognize new rights or apply ancient principles to modern circumstances. They bear the cost of operating within an originalist doctrinal framework where their interpretations are doctrinally marginalized and reversed on appeal; they must suppress their preferred interpretive method to maintain institutional legitimacy and avoid reversal. Their dissenting opinions are outvoted in originalist-dominated courts.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, constrained, national).

% Citizens asserting rights not explicitly listed in the Constitution's text (privacy in intimate decisions, bodily autonomy, dignity interests in recognition and equal respect, etc.). Under the originalist constraint, they must show that the Framers' generation explicitly recognized their right or their constitutional claim fails; they cannot argue from evolved moral understanding or contemporary social necessity. They are trapped because they cannot exit the jurisdiction, cannot change the Constitution alone, and cannot make their case in originalist constitutional law.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, rights_claimants_unenumerated_status, payer,
    powerless, biographical, trapped, national).

% Advocates, scholars, and activists arguing that constitutional protections should recognize contemporary moral principles (gender equality, LGBTQ rights, economic justice, environmental protection, etc.). The originalist constraint forces them to either (a) invest substantial resources in historical research to show their moral position was held in 1787–1868, often requiring creative reinterpretation of ambiguous sources, or (b) pursue constitutional amendment through Article V, a prohibitively expensive alternative requiring political mobilization across states. They are constrained, not trapped, because state-level innovation and political organizing remain open, but federal constitutional protection is locked.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, contemporary_moral_reformers, payer,
    organized, generational, constrained, national).

% The Article V amendment apparatus (Congress supermajority, state ratification by 3/4 supermajority). Under originalism, they are the sole legitimate mechanism for recognizing new rights or updating meaning. They benefit indirectly from the originalist constraint's operation: every pressure for meaning evolution is funneled through their formal supermajority gate, which gives them agenda-setting power over constitutional change and raises the barrier to rights recognition and democratic innovation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_amendment_gatekeepers, beneficiary,
    institutional, civilizational, analytical, national).

% Historians, originalist legal scholars, archivists, and historical method specialists who reconstruct Framing-era thought and evidence. They benefit from the originalist constraint's demand for historical reconstruction; their expertise becomes gatekeeping authority in legal disputes. Their professional standing, citation patterns, and funding rise with originalism's judicial adoption. They can exit by retraining in other legal methods, but doing so costs them the expertise advantage they have accumulated.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, historical_evidence_specialists, beneficiary,
    moderate, biographical, mobile, national).

% State-level elected bodies that might otherwise recognize rights or protections through state constitutional law and statute. They are not excluded from acting (state constitutions remain open), but the originalist constraint on federal courts means their innovations cannot override federal constitutional limits as interpreted by originalist judges, and federal preemption doctrine (itself shaped by originalist principles) constrains state experimentation on issues with interstate implications.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, state_legislatures, excluded,
    powerful, generational, analytical, national).

% Movements advocating for rights and protections discovered or demanded after ratification (abolitionists post-1787, suffragists post-1787, labor organizers post-1787, civil rights activists post-1865, LGBTQ advocates, disability rights advocates, etc.). They are structurally excluded from originalist constitutional arguments because their moral insights and demands postdate the Framing or the Reconstruction era and cannot be projected backward onto the Framers' intent. They can lobby for amendment but cannot argue constitutional necessity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, post_ratification_social_movements, excluded,
    powerless, biographical, trapped, national).

% The institutional apparatus that applies and administers the originalist constraint. Federal courts observe the constraint's operation through appellate review, statutory interpretation, and precedent doctrine, serving as the enforcement mechanism for the meaning-fixed-at-ratification doctrine. They apply originalist methodology in constitutional cases and reverse non-originalist lower court decisions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning across time, jurisdictions, and generational cohorts by anchoring interpretation to a fixed historical moment (ratification), preventing the Constitution from dissolving into competing contemporary readings and allowing judges to base decisions on textual meaning rather than policy preferences. Provides a shared reference point for all interpreters regardless of era or moral outlook, enabling coordinated action on contested constitutional questions.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary elected bodies and moral reformers to (a) the judiciary-as-historians, empowered to reconstruct and enforce Framing-era intent through originalist doctrine, and (b) the Article V supermajority amendment procedure, the only legitimate path to recognized meaning change. Moves decisional power over rights recognition from moral deliberation and contemporary social movements toward historical reconstruction and formal constitutional amendment gatekeepers.
% ABSENT_VOICES: Post-ratification social movements and rights claimants whose moral insights predate their institutional recognition (abolitionists seeking federal protection post-1787, suffragists post-1787, labor advocates, civil rights activists post-1865, LGBTQ and disability advocates) are structurally excluded from originalist constitutional argument. They cannot argue their positions as constitutional necessities; they must either find Framing-era precedent (difficult or impossible) or pursue constitutional amendment. Their exclusion is engineered by the temporal structure of the constraint—historical meaning is fixed at ratification and Reconstruction, and nothing discovered or demanded after those moments can constitute constitutional necessity.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished and courts returned to evolutionary or living-constitution interpretation (or constitutional positivism), judicial recognition of unenumerated rights would resume immediately, contemporary moral understandings would influence constitutional scope, the barrier to rights recognition would drop substantially, and the Article V amendment process would lose its monopoly on constitutional change. State-level constitutional innovation would face fewer federal preemption barriers. Social movements' moral insights would become constituents of constitutional meaning rather than excluded from it. The constitutional order would reorganize around judicial discretion and evolved standards rather than historical fixation, and the distribution of interpretive authority would shift from historians and amendment gatekeepers toward courts and social movements.
% FOUNDING_PROBLEM: Early constitutional interpretation (late 18th century through mid-20th century) was described by originalist advocates as chaotic and inconsistent: different justices read their preferred policy outcomes into vague constitutional text without principled methodology, producing ad-hoc and contradictory precedent that gave courts too much power to rewrite the Constitution. The founding problem was to constrain judicial discretion through a principled, neutral interpretation method (originalism) that would produce predictable, consistent, and non-political constitutional law anchored to historical meaning rather than contemporary judges' views.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal theorists (Scalia, Bork, their successors) and originalist judges (Thomas, Gorsuch, Barrett on the current Supreme Court) attest the founding problem remains live, citing concerns about judicial activism, the need for interpretive discipline, and the danger of judges legislating from the bench. Progressive legal theorists (Balkin, Siegel, Sunstein), living-constitutionalist judges (Kagan, Sotomayor, Jackson on the current Supreme Court), and contemporary constitutional scholars attest the founding problem was substantially solved by the middle of the 20th century (modern statutory interpretation has disciplinary frameworks; non-originalist jurisprudence is neither lawless nor inconsistent across time). Academic historical and empirical analysis from outside the originalist tradition—comparative jurisprudence studies, consistency metrics across methodologies, institutional sociology of courts—provides corroboration that modern constitutional law discipline is not method-dependent. These external witnesses (historians of constitutional law, political scientists, non-originalist legal scholars) support the reading that originalism persists as a constraint motivated by conservative substantive preferences and gatekeeping functions, not by demonstrated necessity.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 at interval end because the constraint's operation extracts substantial decision-making power from contemporary moral deliberation and concentrates it in (a) historical reconstruction (which privileges conservative readings of ambiguous evidence) and (b) the amendment supermajority (which raises the barrier to constitutional change). The constraint is not pure extraction because it genuinely solves a real coordination problem—stabilizing constitutional meaning and constraining judicial discretion—which is why it is claimed as tangled rope rather than snare. However, the beneficiary structure is asymmetric: originalist judges and the amendment supermajority benefit from the constraint's enforcement, while powerless rights claimants and contemporary reformers bear the cost. Suppression rises from 0.55 to 0.71 over the interval because the constraint requires active judicial suppression of evolutionary interpretation and rejection of unenumerated-rights claims; this suppression must be continuously defended (living-constitutionalist judges contest it, lower courts sometimes deviate). Theater rises from 0.12 to 0.28 because originalist justices must increasingly perform historical reconstruction to justify outcomes that align with conservative policy preferences—the reconstruction work becomes increasingly theatrical as the empirical record becomes sparse or ambiguous. The plateau at 0.62–0.71 from t=32 onward reflects a stable regime in which the constraint is mature and defended.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist-judiciary seat, the constraint is a principled, neutral, non-political interpretation method that constrains judicial discretion and stabilizes meaning—a genuine coordination good. From the living-constitutionalist-judges seat, it is an enforced interpretive straitjacket that privileges conservative outcomes by gatekeeping moral progress through historical evidence. From the rights_claimants seat, it is a barrier to justice: their moral claims are delegitimized by historical accident (their rights were not recognized in 1787). From the amendment-gatekeepers seat, it is a beneficial constraint that channels all meaning-change through a supermajority process that protects minority rights. The engine computes each seat's directionality from the structural data: originalist judiciary gets low d (beneficiary), living-constitutionalist judges get high d (target), contemporary reformers get high d (target), rights claimants get maximal d (trapped, powerless, victim). This perspectival divergence is the structural feature the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist constraint operates as tangled rope from the perspective of institutional actors (judiciary, amendment gatekeepers) who benefit from or administer it, and as snare from the perspective of powerless rights claimants and organized reformers who pay its cost without controlling its terms. Originalist judges are beneficiaries with analytical exit (they can change interpretive methodology through judicial evolution or retirement cohort replacement, but doing so is costly and risks institutional legitimacy). Living-constitutionalist judges are constrained-exit targets (they remain in office but their interpretive approach is doctrinally illegitimate). Rights claimants have trapped-exit targets: they cannot exit the jurisdiction, cannot change the Constitution without supermajority consensus they lack, cannot make their case in originalist constitutional law. Contemporary moral reformers are constrained-exit targets: they can pursue state-level innovation or political mobilization for amendment, but federal constitutional protection for their values is locked behind historical evidence and supermajority procedures. Historical evidence specialists have moderate-beneficiary status with mobile exit: their expertise is valuable and their position portable, but abandoning originalism would devalue their skill set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial discretion producing inconsistent, ad-hoc constitutional law) was substantially solved by modern doctrinal development, statutory interpretation frameworks, and precedent structures—even non-originalist jurisprudence operates with discipline and consistency. The originalist constraint persists not because the founding problem remains live, but because it has become institutionalized and benefits identifiable actors (originalist judges, amendment supermajority, historical specialists). The constraint exhibits mandatrophy markers: its primary justification (constraining judicial activism) has been superseded by broader doctrinal maturation and institutional norms; its continued operation serves narrower extractive functions (gatekeeping moral innovation, privileging conservative outcomes through historical evidence control). However, mandatrophy is contested because originalists maintain the founding problem remains live and that activist courts threaten constitutional stability. The contested status means the constraint persists through active defense of its origin story, not through atrophy, but the defense increasingly relies on theater (historical reconstruction of ambiguous evidence) rather than on demonstrable discipline superiority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_reconstruction_ambiguity,
    'When Framing-era historical evidence is sparse, ambiguous, or contested (the typical case for issues the Framers did not explicitly address), what governs originalist reconstruction—the most literal plausible reading, the most common belief among educated persons, or the explicit consensus of recorded debate?',
    'Comparative analysis of originalist jurisprudence when evidence is sparse (unenumerated rights, modern technologies, questions outside the Framers'' contemplation). Empirical study of whether originalist judges'' reconstructions track evidentiary patterns or conservative policy preferences when evidence underdetermines outcome.',
    'If originalist reconstruction is indeterminate (multiple defensible readings from the evidence), the constraint''s claimed discipline advantage collapses—it would then operate more like living constitutionalism with a retroactive historical facade. If reconstruction systematically favors conservative outcomes in ambiguous cases, the extractive function is revealed as primary and the constraint reclassifies toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_reconstruction_ambiguity, empirical, 'Whether originalist methodology produces disciplined, evidence-constrained outcomes or discretionary reconstructions that reach predetermined conservative results.').

omega_variable(
    founding_problem_persistence,
    'Does the founding problem (chaotic, inconsistent judicial constitutional interpretation) remain live, or has it been substantially solved by modern doctrinal development, institutional norms, and precedent culture—such that originalism''s justification has become historically obsolete?',
    'Comparison of constitutional jurisprudence consistency under originalist and non-originalist regimes; analysis of modern statutory interpretation and constitutional law discipline across methodological approaches; assessment of whether non-originalist courts produce more arbitrariness than originalist courts.',
    'If the founding problem is substantially solved, the constraint exhibits mandatrophy: it persists through institutional inertia and benefits to originalist actors, not through current necessity. The classification would remain tangled rope (real coordination function + extraction), but the coordination claim would weaken and the extractive function would become primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the original justification for the originalist constraint remains empirically valid or has been superseded.').

omega_variable(
    unenumerated_rights_historical_contingency,
    'Is the Framers'' failure to enumerate certain rights (privacy, bodily autonomy, dignity interests, etc.) a principled reflection of their considered judgment that such rights lie outside constitutional protection, or an artifact of historical contingency (they did not contemplate the issue)?',
    'Historical analysis of Framing-era thought on the scope of unenumerated rights; study of Ninth Amendment intent; assessment of whether the Framers explicitly rejected modern rights-claimants'' positions or simply did not address them.',
    'If unenumerated-rights exclusion is contingent rather than principled, the originalist constraint''s gatekeeping of contemporary rights claims is revealed as morally arbitrary—not a neutral application of fixed meaning, but an enforced freeze of historical bias. This would support reclassification toward snare or shift assessment of extraction severity. If exclusion is principled, the constraint''s operation reflects authentic Framers'' intent rather than contemporary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_historical_contingency, conceptual, 'Whether unenumerated-rights limitation is a principled Framers'' choice or historical accident.').

omega_variable(
    reading_alternative_framing,
    'Could the originalist reading be reframed as instantiating a different kernel—not ''constitutional authority'' but ''historical textualism as institutional legitimacy''—such that its competitors (living constitutionalism, positivism) are not sibling readings of the same kernel but alternative kernels entirely?',
    'Conceptual analysis of what constitutes the kernel vs. the reading; examination of whether the three readings share a common commitment (they do: ''the Constitution has authoritative meaning'') or whether they are incommensurable frameworks with different kernel commitments.',
    'If the readings are sibling interpretations of one kernel (constitutional authority), then the originalist constraint''s CS structure is correctly modeled with reading_relations. If they are alternative kernels, the committer-frame infrastructure does not apply; each reading instantiates a different constraint with a different kernel grounding. This is a classification question for the constraint itself, not a content omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_alternative_framing, conceptual, 'Whether originalism, living constitutionalism, and positivism are sibling readings of one kernel or alternative kernel frameworks.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression measured in the constraint (living-constitutionalist judges suppressing their interpretive method) structural (imposed by originalist institutional dominance and precedent) or internalized (living-constitutionalist judges have adopted the originalist framework as legitimate, even if they disagree with it)?',
    'Post-regime analysis: if originalist dominance ends and living-constitutionalist methodology returns to favor, do living-constitutionalist judges maintain originalist self-constraint or immediately revert to evolutionary interpretation? Interviews with dissenting judges about their experience of constraint as external coercion vs. internalized legitimacy.',
    'If suppression is primarily structural, the constraint''s extractive force could be reduced by institutional change (judiciary cohort replacement, constitutional amendment, legislative override). If suppression is partially internalized, the constraint''s persistence depends on continued legitimation narratives, and its vulnerability is lower but its extractive effect (self-enforced constraint) is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the suppression in the originalist regime is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__originalist_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__originalist_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__originalist_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__originalist_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__originalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__originalist_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__originalist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__originalist_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__originalist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__originalist_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__originalist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__originalist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.14).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_recognition_gatekeeping).

% DUAL FORMULATION NOTE:
% The kernel of constitutional authority decomposes into three distinct readings with structurally different ε values and beneficiary/victim structures. The originalist reading (this constraint) fixes meaning at ratification and gates unenumerated rights through historical evidence—high extraction for contemporary reformers. The living-constitutionalist reading evolves meaning with social attitudes and recognizes contemporary rights—lower extraction, higher judicial discretion. The positivist reading emphasizes institutional sources and procedure—different extraction mechanics. All three are linked as sibling readings of the same kernel; they are not one constraint viewed from different angles but three structurally distinct constraints instantiated by different readings of the constitutional authority kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
